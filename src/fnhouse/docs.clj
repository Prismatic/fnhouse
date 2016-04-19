(ns fnhouse.docs
  "Proof-of-concept, ultra-primitive HTML doc generation from a fnhouse API spec,
   including schemas and per-namespace handler doc pages."
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [fnhouse.schemas :as schemas]
   [fnhouse.handlers :as handlers])
  (:import
   [java.util.regex Pattern Matcher]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private: minimal implementation of hiccup templating (to avoid external dependencies).

(defn style [m]
  (str
   (->> (sort m)
        (map (fn [[k v]] (str (name k) ":" v)))
        (str/join ";"))
   ";"))

(defn emit-attribute [k v]
  (cond (or (string? v) (keyword? v) (integer? v))
        (str "\"" (.replaceAll ^String (name v) "\"" "&quot;") "\"")

        (and (= k :style) (map? v))
        (emit-attribute k (style v))

        :else (throw (RuntimeException. (str "Can't emit attribute: " (pr-str [k v]))))))

(defn emit-attributes [attr]
  (str/join " " (map (fn [[k v]] (str (name k) "=" (emit-attribute k v))) attr)))

(defn emit-open-tag
  ([tag] (emit-open-tag tag nil))
  ([tag attr]
     (str "<" (name tag)
          (when attr (str " " (emit-attributes attr)))
          ">")))

(defn emit-close-tag [t]
  (str "</" (name t) ">"))

(declare render)

(defn render-helper [form]
  (cond
   (string? form) form
   (keyword? form) (emit-open-tag form)
   (coll? form)
   (when-not (empty? form)
     (let [tag (first form)]
       (if (coll? tag) (apply render form)
           (let [attr (when (map? (second form)) (second form))
                 tail (if (map? (second form)) (rest (rest form)) (rest form))]
             (str (emit-open-tag tag attr)
                  (apply render tail)
                  (emit-close-tag tag))))))
   :else (str form)))

(defn ^String render [& forms]
  (str/join " " (map render-helper forms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private: helpers for producing API docs

(defn walk
  "A version of clojure.walk/walk, extended to work on records."
  [inner outer form]
  (cond
   (list? form) (outer (apply list (map inner form)))
   (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
   (seq? form) (outer (doall (map inner form)))
   (instance? clojure.lang.IRecord form) (outer (into form (map inner form)))
   (coll? form) (outer (into (empty form) (mapv inner form)))
   :else (outer form)))

(defrecord LinkedSchema [schema-name]
  s/Schema
  (spec [this] this)
  (explain [this] [[[schema-name]]]))

(declare extract-schemas)

(defn extract-schemas
  "Extract schemas, replacing defschemas with references to name to be replace with links
   using dirty-hack below."
  [schemas-atom form]
  (if-let [n (s/schema-name form)]
    (do
      (when-not (@schemas-atom n)
        (swap! schemas-atom assoc (str n) (extract-schemas schemas-atom (with-meta form {}))))
      (->LinkedSchema (str n)))
    (walk (partial extract-schemas schemas-atom) identity form)))

(defn dirty-hack
  "Replace linked schemas in the HTML output with links to the corresponding schemas."
  [s]
  (let [p (Pattern/compile "\\[\\[\\[\"(.*?)\"\\]\\]\\]")]
    (loop [s s]
      (let [m (.matcher p s)]
        (if (.find m)
          (let [schema-name (.group m 1)]
            (->> (format "<a href=\"schemas#%s\">%s</a>" schema-name schema-name)
                 (.replaceFirst m)
                 recur))
          s)))))

(defn pprint-str [x]
  (let [^String raw (if (string? x) x (with-out-str (pprint/pprint x)))]
    (-> raw
        (.replace "\n" "<br>")
        (.replace " " "&nbsp;")
        dirty-hack)))

(defn generate-docs [schemas-atom handler-infos]
  (let [explain-walk #(s/explain (extract-schemas schemas-atom %))]
    (aconcat
     (for [handler-info (sort-by (juxt :path :method) handler-infos)]
       (letk [[path method description [:request uri-args query-params body] responses
               resources source-map annotations]
              handler-info]
         [[:h3 (format "%s %s" (-> method name (.toUpperCase)) path)]
          [:table {:border "1" :cellspacing "0" :cellpadding "10"}
           (when description
             [:tr [:td "Description"] [:td (pprint-str (.replaceAll ^String description "\\s+" " "))]])
           (when (seq annotations)
             [:tr [:td "Annotations"] [:td (pprint-str annotations)]])
           (when uri-args
             [:tr [:td "URI Args"] [:td (when uri-args (pprint-str (explain-walk uri-args)))]])
           (when query-params
             [:tr [:td "Query Params"] [:td (when query-params (pprint-str (explain-walk query-params)))]])
           (when body
             [:tr [:td "Post Body"] [:td (pprint-str (explain-walk body))]])
           [:tr [:td "Responses"] [:td (pprint-str (map-vals explain-walk responses))]]
           [:tr [:td "Source"] [:td (handlers/source-map->str source-map)]]]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn all-docs
  "Generate HTML for handlers and schemas from a sequence of HandlerInfo descriptions."
  [handler-infos :- [schemas/HandlerInfo]]
  (let [schemas-atom (atom {})]
    {:handlers (render (generate-docs schemas-atom handler-infos))
     :schemas (render
               [:table {:border "1" :cellspacing "0" :cellpadding "10"}
                (for [[k v] (sort-by key @schemas-atom)]
                  [:tr [:td [:a {:name k} k]] [:td (pprint-str (s/explain v))]])])}))

(s/defn write-docs!
  "Write handlers.html and schemas.html to output-dir from sequence of HanderInfo descriptions."
  [handler-infos :- [schemas/HandlerInfo]
   output-dir :- String]
  (doseq [[k html] (all-docs handler-infos)]
    (spit (format "%s/%s.html" output-dir (name k)) html)))

(defnk $:page$GET
  "Generate HTML for docs, given the output of all-docs as :api-docs resource"
  {:responses {200 String}}
  [[:request [:uri-args page :- (s/enum :handlers :schemas)]]
   [:resources api-docs]]
  {:headers {"Content-Type" "text/html; charset=UTF-8"}
   :body (safe-get api-docs page)})
