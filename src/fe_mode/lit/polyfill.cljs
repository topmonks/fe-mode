(ns fe-mode.lit.polyfill
  (:require
    ["lit-html/polyfills/template_polyfill" :refer [initTemplatePolyfill]]))

(defn init-template-polyfill
  "lit-html use cases. It provides an alternate route in case <template> is not
   natively supported.
   Please note that nested template, cloning template node and innerHTML getter
   do NOT work with this polyfill.
   If it can not fullfill your requirement, please consider using the full
   polyfill: https://github.com/webcomponents/template"
  ([] (init-template-polyfill false))
  ([forced] (initTemplatePolyfill forced)))
