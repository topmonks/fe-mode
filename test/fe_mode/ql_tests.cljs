(ns fe-mode.ql-tests
  (:require
    [goog.dom]
    [clojure.test :as test :refer [deftest testing]]
    [fe-mode.lit :as lit]
    [fe-mode.ql :as ql :refer-macros [get-query]]
    ["lit-html/lib/template-result" :refer [SVGTemplateResult TemplateResult]]))

(deftest actualize-test
  (testing "non-sequences evaluate to themselves"
    (test/is (= (#'ql/actualize 1) 1)))
  (let [x (atom 0)
        coll (for [_ (range 10)]
               (swap! x inc))]
    (testing "coll is still lazy"
      (test/is (= @x 0)))
    (#'ql/actualize coll)
    (testing "coll is fully evaluated"
      (test/is (= @x 10)))))

;; parsing

(defmulti read (fn [a & args] (first a)))
(defmulti mutate (fn [a & args] (first a)))
(defmulti remote (fn [a & args] (first a)))
(defmulti sync (fn [a & args] (first a)))

(defn parse-with [fun query-term]
  (remove-all-methods read)
  (remove-all-methods mutate)
  (remove-all-methods remote)
  (fun)
  (#'ql/parse-query-term query-term {}))

(deftest parse-query-test
  (reset! ql/mount-info {:parsers {:read read
                                   :mutate mutate
                                   :remote remote}
                         :state (atom {})})
  (testing "a read parser result is returned"
    (test/is (= (parse-with (fn []
                              (defmethod read :foo
                                [query-term env state]
                                42))
                            [:foo])
                42)))
  (testing "A mutate function returns a result, but also performs mutations"
    (let [x (atom 0)]
      (parse-with (fn []
                    (defmethod mutate :bar!
                      [query-term env state]
                      (swap! x inc)))
                  [:bar!])
      (test/is (= @x 1))))
  (testing "If no parser is provided, an error is thrown"
    (test/is (thrown-with-msg?
               js/Error
               #"No method in multimethod"
               (parse-with (fn []) [:foo]))))

  (testing "A parser can call parse children for recursive parsing"
    (test/is (= (map #(dissoc % ::ql/env ::ql/query)
                     (parse-with (fn []
                                   (defmethod read :animals
                                     [query-term env state]
                                     (for [animal-id (range 3)]
                                       (ql/parse-children query-term (assoc env :animal-id animal-id))))
                                   (defmethod read :name
                                     [query-term env state]
                                     ({0 :duck 1 :cat 2 :dog} (:animal-id env))))
                                 [:animals {} [:name]]))
                [{:name :duck} {:name :cat} {:name :dog}]))))

(deftest splice-in-seqs-test
  (test/is (= (#'ql/splice-in-seqs [:foo (list :bar :baz)])
              [:foo :bar :baz])
           (= (#'ql/splice-in-seqs [:foo nil :baz])
              [:foo :baz])))

(deftest normalize-query-test
  (test/is (= (#'ql/normalize-query [[:foo [:bar]] [:baz]])
              [[:foo {} [:bar {}]] [:baz {}]]))
  (test/is (= (#'ql/normalize-query [[:foo {} (list [:bar {} [:qux nil (list [:a] [:b])]])] [:baz]])
              [[:foo {} [:bar {} [:qux {} [:a {}] [:b {}]]]] [:baz {}]])))

(defn foo {:query [[:foo {}]]} [])

(deftest get-query-test
  (test/is (= (get-query foo)
              [[:foo {}]])))

(deftest mutation-query-test
  (test/is (= (#'ql/mutation-query-term? [:foo])
              false))
  (test/is (= (#'ql/mutation-query-term? [:foo!])
              true)))

(defn parse-remote-with [fun query]
  (remove-all-methods remote)
  (fun)
  (#'ql/parse-query-remote query))

(deftest parse-query-remote-test
  (reset! ql/mount-info {:parsers {:remote remote}
                         :state (atom nil)
                         :renderer identity})
  (testing "If the remote returns the query, then we get our query back"
    (test/is (= (parse-remote-with
                  (fn []
                    (defmethod remote :foo
                      [query state env]
                      query))
                  [[:foo]])
                [[:foo {}]])))
  (testing "If there are no remotes, we just get an empty seq"
    (test/is (= (parse-remote-with
                  (fn [])
                  [[:foo]])
                nil)))
  (testing "We can parse child queries when parsing a remote query, and parsing functions can modify the query"
    (test/is (= (parse-remote-with
                  (fn []
                    (defmethod remote :foo
                      [query state env]
                      (ql/parse-children-remote query env))
                    (defmethod remote :bar
                      [query state env]
                      [:bar {:baz 42}]))
                  [[:foo {} [:bar]]])
                [[:foo {} [:bar {:baz 42}]]]))))

(defn parse-sync-with [fun query-term result]
  (remove-all-methods sync)
  (fun)
  (#'ql/parse-query-term-sync query-term result {}))

(deftest parse-query-term-sync-test
  (let [state (atom nil)]
    (reset! ql/mount-info {:parsers {:sync sync}
                           :state state
                           :renderer identity})
    (testing "The sync merges the result into the state"
      (parse-sync-with
        (fn []
          (defmethod sync :foo
            [query-term result env state-atom]
            (reset! state-atom result)))
        [:foo {}]
        42)
      (test/is (= @state 42)))
    (testing "If a read query is missing a sync, an error is thrown"
      (test/is
        (thrown-with-msg?
          ExceptionInfo
          #"\[IronQl\] Missing sync parser.*"
          (parse-sync-with (fn [])
                           [:foo {}]
                           42))))
    (testing "Remote mutations are permitted without a sync parser"
      (test/is
        (any?
          (parse-sync-with
            (fn []
              ;; a default method is required
              (defmethod sync :default [a b c d]))
            [:foo! {}]
            42))))
    (testing "Here we are calling child sync functions recursively. Note that lazy seqs will be immediately be made un-lazy by qlkit."
      (reset! state {})
      (parse-sync-with
        (fn []
          (defmethod sync :foo
            [query-term result env state-atom]
            (map-indexed (fn [index item]
                           (ql/parse-children-sync query-term item (assoc env :id index)))
                         result))
          (defmethod sync :bar
            [query-term result env state-atom]
            (swap! state-atom assoc (:id env) result)))
        [:foo {} [:bar]]
        [{:bar :red} {:bar :green} {:bar :blue}])
      (test/is (= @state {0 :red 1 :green 2 :blue})))))

(deftest map-delta-test
  (let [check (fn [map1 map2]
                (test/is (= (merge map1 (#'ql/map-delta map1 map2)) (merge map1 map2))))]
    (check {:a 1} {:b 2})
    (check {:a 1} {:a 2 :b 2})
    (check {:a 1 :b 2} {:a 2 :b 2})
    (test/is (= (#'ql/map-delta {:a 1} {:a 1})
                {}))))

(deftest root-query-test
  (testing "If we're at the root level and the environment is empty, just evaluates to itself"
    (test/is (= (#'ql/root-query {} [[:foo]])
                [[:foo]])))
  (testing "Here, the :bar parser set an env variable of id=55, need to add that to the root query"
    (test/is (= (#'ql/root-query
                  {::ql/parent-env {::ql/query-key :bar
                                    :id 55}}
                  [[:foo]])
                [[:bar {:id 55} [:foo]]])))
  (testing "If the environment has nested parent environments, all env variables end up in the query, but with duplication removed"
    (test/is (= (#'ql/root-query
                  {::ql/parent-env {::ql/query-key :bar
                                    ::ql/parent-env {::ql/query-key :baz
                                                     :id-b 66
                                                     :id-a 77}
                                    :id-a 55
                                    :id-b 66}}
                  [[:foo]])
                [[:baz {:id-b 66, :id-a 77} [:bar {:id-a 55} [:foo]]]]))))

(deftest mount-test
  (ql/mount {:state (atom 5)
             :renderer identity})
  (test/is (= @(:state @ql/mount-info) 5)))

(deftest perform-remote-query-test
  (let [state (atom {:foos {}})]
    (ql/mount {:remote-handler (fn [query callback]
                                 (callback [{:bar 3 :baz 42}]))
               :parsers {:sync sync}
               :state state
               :renderer identity})
    (remove-all-methods sync)
    (defmethod sync :foo
      [query-term result env state-atom]
      (ql/parse-children-sync query-term result (assoc env :foo-id 7)))
    (defmethod sync :bar
      [query-term result env state-atom]
      (swap! state-atom assoc-in [:foos (:foo-id env) :bar] result))
    (defmethod sync :baz
      [query-term result env state-atom]
      (swap! state-atom assoc-in [:foos (:foo-id env) :baz] result))
    (ql/perform-remote-query [[:foo {} [:bar] [:baz]]])
    (test/is (= @state {:foos {7 {:bar 3, :baz 42}}}))))

(deftest aggregate-read-queries-test
  (test/is (= (#'ql/aggregate-read-queries [[:foo {}]]) [[:foo {}]]))
  (test/is (= (#'ql/aggregate-read-queries [[:foo {} [:bar {}]] [:foo {} [:bar {}]]]) [[:foo {} [:bar {}]]]))
  (test/is (= (#'ql/aggregate-read-queries [[:foo {} [:bar {}]] [:foo {} [:baz {}]]]) [[:foo {} [:bar {}] [:baz {}]]]))
  (test/is (= (#'ql/aggregate-read-queries [[:foo {} [:bar {} [:derp1 {}]]] [:foo {} [:bar {} [:derp2 {}]]]]) [[:foo {} [:bar {} [:derp1 {}] [:derp2 {}]]]]))
  (test/is (thrown-with-msg?
             js/Error
             #"query terms with empty and non-empty params cannot be merged."
             (#'ql/aggregate-read-queries [[:foo {} [:bar {}]] [:foo {:a 1} [:bar {}]]])))
  (test/is (= (#'ql/aggregate-read-queries [[:foo {:b 2} [:bar {}]] [:foo {:a 1} [:bar {}]]]) [[:foo {:b 2, :a 1} [:bar {}]]]))
  (test/is (= (#'ql/aggregate-read-queries [[:foo {}] [:baz {}] [:baz {}] [:bar! {}] [:foo {}]]) [[:foo {}] [:baz {}] [:bar! {}] [:foo {}]]))
  (test/is (thrown-with-msg?
             js/Error
             #"query terms with params containing identical keys that have different non-sequence values cannot be merged."
             (#'ql/aggregate-read-queries [[:foo {:a 2} [:bar {}]] [:foo {:a 1} [:bar {}]]])))
  (test/is (= (#'ql/aggregate-read-queries [[:foo {:a #{2}} [:bar {}]] [:foo {:a #{1}} [:bar {}]]]) [[:foo {:a #{1 2}} [:bar {}]]])))

;; rendering

(defonce root-el (goog.dom/createElement "div"))
(when-not (js/document.body.contains root-el)
  (goog.dom/appendChild js/document.body root-el))

(defn bold [t]
  (lit/html "<b>" t "</b>"))

(defn list-item [t]
  (lit/html "<li>" (bold t) "</li>"))

(defn unordered-list [items]
  (lit/html
    "<ul>"
    (map list-item items)
    "</ul>"))

(defn root {:query [[:items]]} [{:keys [items]}]
  (lit/html
    "<div>"
    (unordered-list items)
    "</div>"))

(defonce state (atom {:items [1, 2, 3, 4, 5]}))

(defn reader [[k _] _ state]
  (get state k []))

(deftest lit-ql-rendering
  (testing "Render should populate root element"
    (test/is (nil? (ql/mount
                     {:state state
                      :parsers {:read reader}
                      :query (get-query root)
                      :renderer #(lit/render (root %) root-el)})))
    (test/is (= (.-length (goog.dom/getChildren root-el)) 1))
    (test/is (= (.-length (goog.dom/getElementsByTagName "li" root-el)) 5))))
