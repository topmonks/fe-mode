(ns fe-mode.lit-tests
  (:require
    [goog.dom]
    [clojure.test :as test :refer [deftest testing]]
    [fe-mode.lit :as lit]
    ["lit-html/lib/template-result" :refer [SVGTemplateResult TemplateResult]]))

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

(defn ^{:query [[:items]]} root [{:keys [items]}]
  (lit/html
    "<div>"
    (unordered-list items)
    "</div>"))

(defonce state (atom {:items [1, 2, 3, 4, 5]}))

(deftest lit-html-rendering
  (testing "Render should populate root element"
    (test/is (nil? (lit/render (root @state) root-el)))
    (test/is (= 1 (.-length (goog.dom/getChildren root-el))))
    (test/is (= 5 (.-length (goog.dom/getElementsByTagName "li" root-el))))))


