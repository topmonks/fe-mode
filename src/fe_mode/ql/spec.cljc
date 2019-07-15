;   Copyright (c) Conrad Barski. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; This code is derived work of original Qlkit by Conrad Barski decoupled from React.

(ns fe-mode.ql.spec
  (:require
    [clojure.spec.alpha :as s]))

(def ^:dynamic *defining-component?*)

(defn mutation-query? [k]
  (= \! (last (name k))))

(defn contains-mutation? [query-term]
  (->> query-term
       (filter vector?)
       (map first)
       (some mutation-query?)))

(s/def ::component-definition-rules
  (fn [query-term]
    (if *defining-component?*
      (not (contains-mutation? query-term))
      true)))

(s/def ::query-term
  (s/spec (s/and vector?
                 ::component-definition-rules
                 (s/cat
                   :tag keyword?
                   :attrs (s/? map?)
                   :children (s/* ::query-term)))))

(s/def ::query
  (s/spec (s/and vector? (s/cat :query (s/* ::query-term)))))

(defn query-spec [query & [defining-component?]]
  (binding [*defining-component?* defining-component?]
    (when-not (s/valid? ::query query)
      (throw (ex-info (str "Invalid query: \n" query "\n" (with-out-str (s/explain ::query query))) {})))))
