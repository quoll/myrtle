(ns ^{:doc "Basic RDF conversion of tables and TTL output functions"
      :author "Paula Gearon"}
    myrtle.core
    (:require [clojure.string :as s]
              [clojure.instant :as i]
              #?(:clj [clojure.java.io :as io]
                 :clr [clojure.clr.io :as io]))
    (:import #?(:clj [java.io Writer]
                :clr [System.IO StreamWriter])))

(def base-context
  {"xsd" "https://www.w3.org/TR/xmlschema-2/#"
   "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   "rdfs" "http://www.w3.org/2000/01/rdf-schema#"})


(defn capitalize
  "Converts a name into CamelCase, using hyphens, underscores, dollars and spaces to separate words.
  Accepts a boolean parameter to indicate if the first word should be capitalized. Defaults to true."
  ([s] (capitalize s true))
  ([s include-first]
   (let [[fpart & rparts :as parts] (s/split s #"[-_$ ]")]
     (->> (if include-first parts rparts)
          (map #(str (s/upper-case (subs % 0 1)) (subs % 1)))
          (apply str (when-not include-first fpart))))))

(defn to-records
  "Converts table data to records.
  The format of the table data is a sequence of vectors, where the first element is a header
  and the remaining elements are vectors containing data corresponding to the header.
  This is compatible with data returned from clojure.data.csv.
  Returns a seq of array-maps that maintain the order of the columns."
  [[header & data :as table]]
  (map #(apply array-map (interleave header %)) data))

(defn record-entity-converter
  "Returns a function that converts a record created with a zipmap into a format
  that can be easily converted to RDF.
  The resulting record has an `:id` field to represent the resource, an `:rdf/type` for the record type,
  possibly an `:rdfs/label`, and may have values converted into appropriate datatypes.
  The spec argument describes how this can be accomplished.
  spec: A map containing:
    - context: a map of localnames to namespaces. The type namespace and property namespaces must be included.
    - key: the name of the column containing the primary key of the record.
    - label: the name of the column containing the label of the record.
    - type: The name of the record type. This is used for the localname and the value, as in `type:Type`
    - ptype: The localname for the properties. This is used for property names, as in `ptype:theProperty`
    - colmap: A map of column names to conversion functions.
              Only required for functions that need to be parsed into a type. Optional."
  [{:keys [context type ptype key label colmap] :as spec}]
  (when-not (get context type)
    (throw (ex-info "Record type is needed as a localname in the context"
                    {:context-type (:type context) :type type})))
  (when-not (get context ptype)
    (throw (ex-info "Record property type is needed as a localname in the context"
                    {:context-prop-type (:ptype context) :ptype ptype})))
  (when-not key (throw (ex-info "Primary key column must be set" {:key key})))
  (let [dconj! (fn [t a b] (conj! (conj! t a) b))
        type-name (keyword type (capitalize type))
        ;; an optional leader string that may appear on some column names
        tleader (str type "$_")
        tleader-len (count tleader)]
    (fn [recrd]
      (let [lbl (get recrd label)
            row (cond-> (transient [:id (keyword type (get recrd key)) :rdf/type type-name])
                  (seq lbl) (dconj! :rdfs/label lbl))
            full-row (reduce-kv (fn [r k v]
                                  (if (seq v)
                                    ;; some table columns may begin with the table name leader
                                    (let [kn (if (s/starts-with? k tleader) (subs k tleader-len) k)]
                                      (dconj! r (keyword ptype kn) ((get colmap k identity) v)))
                                    r))
                                row
                                (dissoc recrd key label))]
        (apply array-map (persistent! full-row))))))

(def ^:private emap {\newline \n, \tab \t, \return \r, \backspace \b, \formfeed \f})

(defn escape
  "Creates escape sequences for Turtle strings."
  [s]
  (s/replace s #"[\u0008\t\n\r\f\"'\\]" #(str "\\" (emap (first %) %))))

(defn ref-str
  "Converts a reference of some type into a string representation."
  [r]
  (cond
    (keyword? r) (if (= :rdf/type r) "a" (str (namespace r) ":" (name r)))
    (uri? r) (str "<" r ">")
    (inst? r) (str (subs (pr-str r) 6) "^^<xsd:dateTime>")
    :default (str \" (escape r) \")))

(defn emit-entity
  "Writes an entity to the output stream.
  w: the output stream writer.
  entity: A map with an `:id` key to identify the object, and other keys being URIs or keywords acting as QNames."
  [w {id :id :as entity}]
  (let [id-label (ref-str id)
        indent (apply str (repeat (inc (count id-label)) \space))
        property-objects (seq (dissoc entity :id))
        write (fn [& args] (doseq [a args] (.write w a)))]
    (when (seq property-objects)
      (let [[p o] (first property-objects)]
        (write id-label " " (ref-str p) " " (ref-str o)))
      (doseq [[p o] (rest property-objects)]
        (write " ;\n" indent (ref-str p) " " (ref-str o)))
      (write " .\n"))))

;; TTL files also need a header.
(defn emit-header
  "Writes a context header to the output stream.
  w: the output stream writer.
  context: A map of localname strings to namespace strings."
  [w context]
  (let [write (fn [v] #?(:clj (.write ^Writer w v) :clr (.Write ^StreamWriter w v)))]
    (doseq [[p n] context]
      (write "@prefix ")
      (write p)
      (write ": <")
      (write n)
      (write "> .\n"))
    (write "\n")))

(defn write-ttl
  "Writes a sequence of entities to a TTL file, using a given context.
  This does not attempt to use the context to shorten URIs, since that would take too long.
  writer A writer to send the TTL to.
  context: A map of localname strings to namespace strings.
  entities: A seq of maps containing 3 types of keys:
            `:id`: the identifier for this entity.
            keyword: A property that will be expressed as a QName.
            URI: A property that will be expressed as a URI."
  [writer context entities]
  (emit-header writer context)
  (doseq [entity entities]
    (emit-entity writer entity)))

(defn write-ttl-file
  "Writes a sequence of entities to a TTL file. See write-ttl for details."
  [filename context entities]
  (with-open [writer #?(:clj (io/writer filename) :clr (io/text-writer filename))]
    (write-ttl writer context entities)))

