;   Copyright (c) Conrad Barski. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; This code is derived work of original Qlkit by Conrad Barski decoupled from React.

(ns fe-mode.ql
  #?(:cljs (:require-macros [fe-mode.ql]))
  (:require
    [fe-mode.ql.spec :as spec]))

(defn safe-deref [state]
  (let [derefable? #?(:clj (instance? clojure.lang.IDeref state)
                      :cljs (satisfies? IDeref state))]
    (if derefable?
      @state
      state)))

(defn warning [msg]
  (throw (ex-info msg {})))

(def mount-info (atom {}))

(defn- actualize
  "This function makes sure all shallow lazy sequences are expanded,
   IF there is a lazy sequence."
  [x]
  (if (seq? x)
    (doall x)
    x))

(defn- mutation-query-term?
  "Indicates if the mutation term is a mutation- Note that this is
   a check for a 'shallow' mutation, by design subterms could still
   be a mutation."
  [query-term]
  (= \! (last (name (first query-term)))))

(defn get-fn [f & args]
  (if (instance? #?(:cljs MultiFn
                    :clj  clojure.lang.MultiFn)
                 f)
    (->> (apply (let [v #?(:cljs (.-dispatch-fn f)
                           :clj (.-dispatchFn f))]
                  v)
                args)
         (get-method f))
    f))

(defn- parse-query-term
  "Parses a single query term, i.e. something in the form [:person {} [:person/name] [:person/age]].
   The environment is used to pass info from parent queries down to child queries."
  [query-term env]
  (let [{:keys [state parsers]} @mount-info
        {:keys [read mutate remote]} parsers
        mutate-fn (get-fn mutate query-term env state)]
    (if (or (not (mutation-query-term? query-term))
            mutate-fn
            (get-fn remote query-term state))
      (actualize (cond
                   (mutation-query-term? query-term)
                   (when mutate-fn (mutate-fn query-term env state))
                   read
                   (read query-term env (safe-deref state))
                   :else nil))
      (warning (str "[IronQl] mutate! query must have either a mutate or a remote parser: "
                    (pr-str query-term))))))

(defn parse-query
  "Parses an entire query, i.e. something with multiple query terms,
   such as [[:person {} [:person/name]] [:widget {} [:widget/name]]].
   The output of 'parse-query' is meant to be sent by the server to
   the client to pass back query results."
  ([query env]
   (doall (for [query-term query]
            (parse-query-term query-term env))))
  ([query]
   (parse-query query {})))

(defn- parse-query-into-map
  "This parses a query so the results are in nested maps for easy access.
   This is used for all internal query parsing in cases where there are
   unique keys for query terms, which is true except for the root query
   returned by 'transact!', which can have duplicate keys in order
   to guarantee side-effect order."
  [query env]
  (into #?(:clj  {} ;;Only client components need local env/query, so let's strip them for server-side requests
           :cljs {::env env ::query query})
        (map vector (map first query) (parse-query query env))))

(defn parse-children
  "Takes a query and the environment and triggers all children.
   The key goal here is that environmental values passed to children need
   to be marked with the parent query they originated from, to aid
   in generating a well-formed remote query."
  [query-term env]
  (parse-query-into-map
    (drop 2 query-term)
    (assoc env ::parent-env (assoc env ::query-key (first query-term)))))

(defn- splice-in-seqs
  "Supports 'seq splicing' and 'nil eliding'... i.e. converts [:foo (list :bar :baz)]
   into [:foo :bar :baz] and [:foo nil :baz] into [:foo :baz]"
  [coll]
  (reduce (fn [acc item]
            (cond (seq? item) (vec (concat acc (splice-in-seqs item)))
                  item (conj acc item)
                  :else acc))
          []
          coll))

(defn- normalize-query-helper
  "Splices in seqs recursively, and also puts in missing empty attribute lists."
  [query]
  (for [item (splice-in-seqs query)]
    (let [maybe-params (second item)
          [nam params children] (if (and maybe-params (and (not (vector? maybe-params)) (not (seq? maybe-params))))
                                  [(first item) (second item) (drop 2 item)]
                                  [(first item) {} (rest item)])]
      (apply vector
             nam
             params
             (normalize-query-helper children)))))

(defn- aggregate-params
  "Aggregates params accross similar query terms with different params.
   If one of the query terms is the empty query {} it must be the only query
   (or the params will become 'overspecialized')
   Otherwise, param key names across terms must be distinct
   OR identical in value OR they must both have collection values
   (so that concating them is possible)"
  [params-coll]
  (let [[param & more] (distinct params-coll)]
    (reduce
      (fn [acc item]
        (if (and (seq acc) (seq item))
          (into
            {}
            (for [key (keys (merge acc item))]
              [key
               (let [acc-val (acc key)
                     item-val (item key)]
                 (cond (not acc-val) item-val
                       (not item-val) acc-val
                       (= acc-val item-val) acc-val
                       (and (coll? acc-val) (coll? item-val)) (apply conj acc-val item-val)
                       :else (throw (ex-info "query terms with params containing identical keys that have different non-sequence values cannot be merged." {}))))]))
          (throw (ex-info "query terms with empty and non-empty params cannot be merged." {}))))
      param
      more)))

(defn- aggregate-read-queries
  "If two query terms of the same type exist in the query, and they are not separated
   by a mutation query, we combine them, recursively."
  [query]
  (when-let [[query-term & more] (seq query)]
    (if (mutation-query-term? query-term)
      (cons query-term (aggregate-read-queries more))
      (let [[nam params & children]
            query-term
            {:keys [extra-children extra-params remaining]}
            (reduce
              (fn [{:keys [finished] :as acc} [cur-nam cur-params & cur-children :as item]]
                (cond finished
                      (update acc :remaining conj item)
                      (mutation-query-term? item)
                      (-> acc
                          (assoc :finished true)
                          (update :remaining conj item))
                      (= cur-nam nam)
                      (-> acc
                          (update :extra-children
                                  (fn [children]
                                    (apply conj children cur-children)))
                          (update :extra-params
                                  (fn [params]
                                    (conj params cur-params))))
                      :else
                      (update acc :remaining conj item)))
              {:extra-children []
               :extra-params [params]
               :remaining []
               :finished false}
              more)]
        `[[~nam
           ~(aggregate-params extra-params)
           ~@(aggregate-read-queries (concat children extra-children))]
          ~@(aggregate-read-queries remaining)]))))

(defn- normalize-query [query]
  (aggregate-read-queries (normalize-query-helper query)))

(defn- parse-query-remote
  "This parses a query and sends off its parts to any 'remote' query handlers.
   Returns another query (the query to send to the server) as a result."
  [query]
  (normalize-query
    (reduce
      (fn [acc item]
        (let [{:keys [state parsers]} @mount-info
              {:keys [remote]} parsers
              state' (safe-deref state)]
          (if (get-fn remote item state')
            (if-let [v (remote item state')]
              (conj acc v)
              acc)
            acc)))
      []
      query)))

(defn get-query* [var]
  (seq (:query (meta var))))

#?(:clj
   (defmacro get-query
     "Returns the query for a class. Note that in IronQl, queries are
      not changed at runtime and hence just retrieved at the class-level."
     [component]
     `(get-query* (var ~component))))

(defn parse-children-remote
  "This is a function you can use within a remote query parser to iteratively
   execute the children of the query."
  [[dispatch-key params & chi :as query]]
  (let [chi-remote (parse-query-remote chi)]
    (when (seq chi-remote)
      (vec (concat [dispatch-key params] chi-remote)))))

(defn- parse-query-term-sync
  "Calls the sync parsers for a query term, which are responsible for merging
   server results into the client state."
  [[key :as query-term] result env]
  (if-let [sync-fun (get-fn (:sync (:parsers @mount-info)) query-term result env (:state @mount-info))]
    (actualize (sync-fun query-term result env (:state @mount-info)))
    (or (mutation-query-term? query-term)
        (warning (str "[IronQl] Missing sync parser but received sync query: "
                      (pr-str query-term))))))

(defn parse-children-sync
  "This function can be called from sync parsers to recursively perform child sync queries."
  [query-term result env]
  (doseq [[key :as child-query-term] (drop 2 query-term)]
    (parse-query-term-sync child-query-term (result key) env)))

(defn- map-delta
  "Finds the minimal X such that (merge map1 X) = (merge map1 map2)"
  [map1 map2]
  (into {} (filter (fn [[k v]] (not= v (map1 k)))) map2))

(defn- root-query
  "Takes a query that is relative to a component in the hierarchy
   and converts it into a query at the root level. Note that each term
   in the original query will be given its own 'root' in the resulting
   query, which is done to control ordering of side effects."
  [env query]
  (for [query-term query]
    (loop [query-term query-term
           env (::parent-env env)]
      (if env
        (let [parent-env (::parent-env env)]
          (recur [(::query-key env)
                  (dissoc (if parent-env
                            (map-delta parent-env env)
                            env)
                          ::parent-env
                          ::query-key)
                  query-term]
                 parent-env))
        query-term))))

(declare refresh)

(defn mount
  "This is used to mount IronQl tied to a dom element
   (or without a dom element, when used on the server.)
   The args map can contain:
     :parsers (the map of parsers)
     :query (The query of the root IronQl component)
     :renderer (Function to render refreshed data passed as argument)
     :state (a state atom) and
     :remote-handler (function to call for sending out changes to the server)
     :server? (flag if this is a server mount).

   Only one mount can be set up on the client, and one on the server."
  [args]
  (assert (map? args) "IronQl needs a Map argument defining the options.")
  (reset! mount-info args)
  (when-not (:server? args)
    (refresh true)))

(defn perform-remote-query
  "This calls the remote handler to process the remote query and offers
  up a callback that is called when the server has returned the results
  from the query."
  [query]
  (when (seq query)
    (if-let [handler (:remote-handler @mount-info)]
      (handler
        query
        (fn [results]
          (doseq [[k v] (map vector query results)]
            (parse-query-term-sync k v {}))
          (refresh false)))
      (warning (str "[IronQl] Missing :remote-handler but received query: " (pr-str query))))))

#?(:clj  (defn- refresh [remote-query?]
           nil)
   :cljs (do
           (defn- refresh
             "Force a redraw of the entire UI. This will trigger local parsers
             to gather data, and optionally will fetch data from server as well."
             [remote-query?]
             (let [query (:query @mount-info)
                   atts (parse-query-into-map query {})
                   {{spec :spec} :parsers} @mount-info]
               (spec/query-spec (vec query))
               (when spec
                 (spec (vec query) :synchronous))
               (when remote-query?
                 (perform-remote-query (parse-query-remote query)))
               ((:renderer @mount-info) atts)))))
