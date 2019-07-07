(ns fe-mode.lit
  (:require
    [methyl.console]
    ["lit-html/lib/default-template-processor" :refer [defaultTemplateProcessor]]
    ["lit-html/lib/directive" :as directive]
    ["lit-html/lib/render" :as lit]
    ["lit-html/lib/template-result" :refer [SVGTemplateResult TemplateResult]]))

(extend-type array
  ICollection
  (-conj [coll o]
    (.push coll o)
    coll))

(def directive directive/directive)
(def directive? directive/isDirective)

(defn html
  "Interprets a template literal as an HTML template that can efficiently
   render to and update a container."
  [& forms]
  (let [strings (into (array) (filter string?) forms)
        values (into (array) (remove string?) forms)]
    (TemplateResult. strings values "html" defaultTemplateProcessor)))

(defn svg
  "Interprets a template literal as an SVG template that can efficiently
   render to and update a container."
  [& forms]
  (let [strings (into (array) (filter string?) forms)
        values (into (array) (remove string?) forms)]
    (SVGTemplateResult. strings values "svg" defaultTemplateProcessor)))

(defn render
  "Renders a template to a container.

   To update a container with new values, reevaluate the template literal and
   call `render` with the new result.

   @param result a TemplateResult created by evaluating a template tag like
       `html` or `svg`.
   @param container A DOM parent to render to. The entire contents are either
       replaced, or efficiently updated if the same result type was previous
       rendered there.
   @param options RenderOptions for the entire render tree rendered to this
       container. Render options must *not* change between renders to the same
       container, as those changes will not effect previously rendered DOM."
  ([result container] (render result container nil))
  ([result container options]
   (methyl.console/time ::render-time)
   (lit/render result container options)
   (methyl.console/time-end ::render-time)))
