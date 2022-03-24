# myrtle
A library for working with RDF Turtle.

## Importing
Myrtle can be included in `deps.edn` with the following in the `:deps` map:
```clojure
com.github.quoll/myrtle {:git/tag "v0.0.1" :git/sha "3763dce"}
```

## Tables
For now, this library has been a collection of tools I have been writing to work with RDBMS tables dumped as CSV files.

To load Myrtle:

```clojure
(require '[myrtle.core :as m])
```

Tables are typically provided as a seq of arrays, with the first array containing the column names:

```clojure
(def data [["id" "name" "age" "email"]
           ["1" "Elizabeth" "20" "liz@longbourn.net"]
           ["2" "Fitzwilliam" "28" "bill@pemberley.com"]
           ["3" "Jane" "22" "jane@longbourn.net"]
           ["4" "Charles" "30" "chuck@netherfieldpark.com"]])
```

These can be converted to string-keyed records with `to-records`:
```clojure
=> (def records (m/to-records data))
=> records
({"id" "1", "name" "Elizabeth", "age" "20", "email" "liz@longbourn.net"}
 {"id" "2", "name" "Fitzwilliam", "age" "28", "email" "bill@pemberley.com"}
 {"id" "3", "name" "Jane", "age" "22", "email" "jane@longbourn.net"}
 {"id" "4", "name" "Charles", "age" "30", "email" "chuck@netherfieldpark.com"})
```
Alternatively, this can be done via `clojure.data.csv` to read the data directly from a file. Note that `to-records` is lazy, so the data must all be consumed before a stream reader is closed (performed with `doall` in this example):
```clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [rdr (io/reader "filename.csv")]
  (doall (to-records (csv/read-csv rdr))))
```

### Specification
After any work you may want to do with them, the records can be converted into entites using a specification.
This requires a context (a map of localname/namespace pairs used in the RDF), a `"key"` column,
a `type` for the datatype being created, and a `ptype` for the localname used for predicates.
Optionally, the spec may also include the column to use for labeling the data, and a map of columns to
functions to be used to parse that column.

The original context can be based on `myrtle.core/base-context` which contains the namespaces for
`rdf`, `rdfs`, and `xsd`.
```clojure
(def context (assoc m/base-context
                    "ppl" "http://pnp.org/people#"
                    "ppl-prop" "http://pnp.org/people/properties#"))
(def spec
 {:context context
  :key "id"
  :type "ppl"  ;; the type must appear in the context map
  :ptype "ppl-prop"  ;; the ptype must appear in the context map
  :colmap {"age" parse-long}})
```
### RDF Conversion
This spec can be used to create a function that converts records into RDF compatible records
that can be easily emitted as TTL:
```clojure
=> (def converter (m/record-entity-converter spec))
=> (def rdf-records (map converter records))
=> rdf-records
({:id :ppl/1,
  :rdf/type :ppl/Ppl,
  :ppl-prop/name "Elizabeth",
  :ppl-prop/age 20,
  :ppl-prop/email "liz@longbourn.net"}
 {:id :ppl/2,
  :rdf/type :ppl/Ppl,
  :ppl-prop/name "Fitzwilliam",
  :ppl-prop/age 28,
  :ppl-prop/email "bill@pemberley.com"}
 {:id :ppl/3,
  :rdf/type :ppl/Ppl,
  :ppl-prop/name "Jane",
  :ppl-prop/age 22,
  :ppl-prop/email "jane@longbourn.net"}
 {:id :ppl/4,
  :rdf/type :ppl/Ppl,
  :ppl-prop/name "Charles",
  :ppl-prop/age 30,
  :ppl-prop/email "chuck@netherfieldpark.com"})
```
Note that Myrtle uses keywords to represent QNames. Also note that the `:id` key is not an RDF property.
This is the equivalent to `"@id"` in [JSON-LD](https://json-ld.org/).

## Writing
Once in this form, the data can be written to a file.
```clojure
(m/write-ttl-file "people.ttl" context rdf-records)
```

## Full Operation
The above steps are useful while trying to clean the data and determine the appropriate spec,
but once this is established, it can all be done in an integrated set of operations:
```clojure
(let [context (merge m/base-context
                     {"ppl" "http://pnp.org/people#"
                      "ppl-prop" "http://pnp.org/people/properties#"})
      converter (m/record-entity-converter
                  {:context context
                   :key "id"
                   :type "ppl"
                   :ptype "ppl-prop"
                   :colmap {"age" parse-long}})]
  (with-open [rdr (io/reader "people.csv")]
    (write-ttl-file
      "people.ttl"
      context
      (map converter (m/to-records (csv/read-csv rdr)))))
```

Distributed under the Eclipse Public License version 2.0.

