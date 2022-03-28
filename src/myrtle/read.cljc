(ns ^{:doc "A TTL reader"
      :author "Paula Gearon"}
    myrtle.read
    (:require [clojure.java.io :as io])
    (:import [java.io Reader Writer]
             [clojure.lang Volatile]))

(def ^:const nl (int \newline))
(def ^:const rt (int \return))
(def ^:const base-key "@base")
(def ^:const achar (int \a))
(def ^:const zchar (int \z))
(def ^:const Achar (int \A))
(def ^:const Zchar (int \Z))
(def ^:const colon (int \:))
(def ^:const lt (int \<))

(defmacro pn_chars_base?
  "Tests if a character is in the PN_CHARS_BASE set. Does not look for surrogates."
  [c]
  (or (and (>= c Achar) (<= c Zchar))
      (and (>= c achar) (<= c zchar))
      (and (>= c 0xc0) (<= c 0xd6))
      (and (>= c 0xd8) (<= c 0xf6))
      (and (>= c 0xf8) (<= c 0x2ff))
      (and (>= c 0x370) (<= c 0x37d))
      (and (>= c 0x37f) (<= c 0x1fff))
      (and (>= c 0x200c) (<= c 0x200d))
      (and (>= c 0x2070) (<= c 0x218f))
      (and (>= c 0x2c00) (<= c 0x2fef))
      (and (>= c 0x3001) (<= c 0xd7ff))
      (and (>= c 0xf900) (<= c 0xfdcf))
      (and (>= c 0xfdf0) (<= c 0xfffd))))

(defmacro casei
  "A variation on `case` that takes constants that are always ints"
  [e & args]
  (let [pairs (partition 2 args)
        la (last args)
        l (mapcat (fn [[k v]]
                    (if (list? k)
                      [(map int k) v]
                      [(int k) v])) pairs)]
    (if (even? (count args))
      `(case ~e ~@l)
      `(case ~e ~@l ~la))))

(defmacro read
  "Reads a single character from a stream"
  [r]
  #?(:clj (.read r)
     :cljr (.Read r)))

(defmacro append
  "appends a single element to a string builder"
  [sb e]
  #?(:clj (.append sb e)
     :clr (.Append sb e)))

(defn read-n
  "Reads to newline, EOF, or n characters, whichever comes first. Returns a string."
  [^Reader r n]
  (let [StringBuilder s]
    (loop [i n c (.read r)]
      (if (or (zero? i) (= -1 c) (= nl c) (= rt c))
        (.toString s)
        (recur (dec i) (read r))))))

(defmacro readc
  "Reads a character that is not whitespace."
  [r]
  (loop [^int c (.read ^Reader r)]
    (case c
      (32 9 10 12 13) (recur (read ^Reader r))
      c)))

(defmacro read-uchar
  "Reads a Unicode escape string.
  r: The reader that has just returned a \\ characater."
  [r sb c]
  (let [encoding (cond
                   (= \u c) (apply str (read r) (read r) (read r) (read r))
                   (= \U c) (apply str
                                   (read r) (read r) (read r) (read r)
                                   (read r) (read r) (read r) (read r))
                   :default (do
                              (append sb)
                              (throw (ex-info (str "Bad unicode sequence: " sb \\ c) {:char c}))))
        codepoint #?(:clj (Integer/parseInt encoding 16)
                     :cljr (Int32/Parse encoding System.Globalization.NumberStyles/HexNumber))]
    (Character/toString codepoint)))

(def non-iri (set (map int (concat (range 33) "<\"{}|^`"))))

(defn read-iri-ref
  "Reads a URI (or IRI) from the reader. The `<` character has already been read."
  [^Reader r]
  (let [sb (StringBuilder.)]
    (loop [^int c (read r)]
      (cond
        \> (str sb)
        (non-iri c) (throw (ex-info (str "Invalid character in IRI: " sb c) {:char c}))
        (= \\ c) (let [c (read r)]
                   (append sb (read-uchar r sb c)))
        :default (append sb (char c))))))

(defn read-pname-ns
  "Reads the prefix in a prefixed name"
  [^Reader r]
  (let [sb (StringBuilder.)]
    (loop [^int c (readc r)]
      (cond
        (= colon c) (str sb)
        (pn-chars-base? c) (do (append sb c) (recur (read r)))
        (Character/isHighSurrogate c) (let [c2 (read r)]
                                        (when-not (Character/isLowSurrogate c2)
                                          (throw (ex-info (str "Bad 2-character Unicode pair after:" sb)
                                                          {:high c :low c2})))
                                        (when (> c 0xdb7f)
                                          (throw (ex-info (str "Character out of range: " c c2)
                                                          {:high c :low c2 :pname (str sb)})))
                                        (append sb c)
                                        (append sb c2)
                                        (recur (read r)))
        (throw (ex-info (str "Character out of range: " c) {:pname (str sb)}))))))

(defn read-base!
  "Reads the parameters of a base directive.
  r: The reader.
  context: a map wrapped in a Volatile. Will be updated with a new `@base` entry."
  [^Reader r context]
  (let [c (readc r)]
    (when-not (= \< c)
      (throw (ex-info (str "Bad IRI in base directive: " c (read-n 80)) {:context @context})))
    (let [u (read-iri-ref r)]
      (vswap! context assoc base-key u))))

(defn read-prefix!
  "Reads the parameters of a prefix directive.
  r: The reader.
  context: a map wrapped in a Volatile. Will be updated with a new entry."
  [^Reader r context]
  (let [pname-ns (read-pname-ns r)
        c (readc r)]
    (when-not (= c lt)
      (throw (ex-info (str "Prefix must have a valid IRI: " c) {:line (str c (read-n r 80))})))
    (let [iri (read-iri-ref r)]
      (vswap! context assoc pname-ns iri))))

(defn read-directive!
  "Reads a directive line, and updates a volatile context map.
  r: the reader.
  context: a map wrapped in a Volatile."
  [^Reader r context]
  (let [start (read-n r 5)]
    (cond
      (= "base " start) (read-base! r context)
      (= "prefi" start) (let [s (read-n r 2)]
                          (when-not (= s "x ")
                            (throw (ex-info (str "Invalid directive: @prefi" s) {:context @context})))
                          (read-prefix! r context))
      (throw (ex-info (str "Invalid directive: " start (read-n 80)) {:context @context}))))
  (when-not (= \. (readc r))
    (throw "Directive must end with a '.' character" {:context @context})))

(defmacro ccons
  "Conses 2 values onto the start of a seq"
  [a b s]
  `(cons ~a (cons ~b ~s)))

(defmacro new-node
  "Creates a new node object from a volatile int value"
  [c]
  (keyword "_" (str "b" (vswap c inc))))

(defprotocol Emitter
  (emit [e s p o] "Emits a subject/predicate/object triple onto the emitter object"))

(extend-protocol Emitter
  Volatile
  (emit [v s p o] (vswap! conj! [s p o]))
  Writer
  (emit [w s p o]
    (.write w s)
    (.write w \space)
    (.write w p)
    (.write w \space)
    (.write w o)
    (.write w \newline)))

(defn sm-read
  [^Reader r context triples]
  (let [n (volatile! 0)]
    (loop [c (readc r) state :newstmt stack ()]
      (case state
        :newstmt (casei c
                        -1 :eof
                        \@ (do
                             ;; update the context
                             (read-directive! r context) ;; also absorbs the .
                             (recur (readc r) state stack))
                        \< (let [uri (read-uri r)]
                             (recur (readc r) :predicate-list (ccons :newstmt uri stack)))
                        \[ (recur (readc r) :predicate-list (ccons :predicate-list (new-node n) stack))
                        \( (let [coll (new-node n)]
                             (recur (readc r) :object (ccons :collection :rdf/first (ccons :predicate-list coll stack))))
                        (let [subject (read-local-or-base r)]
                          (cond
                            (= "BASE " subject) (read-base! r context)
                            (= "PREFIX " subject) (read-prefix! r context))
                          (recur (readc r) :predicate-list (ccons :newstmt subject stack))))
        :object (let [nxt (first stack)
                      offset (if (= :object-list nxt) 1 0) ;; checks for single stack value from :object-list state
                      predicate (nth stack (+ 1 offset))
                      subject (nth stack (+ 3 offset))]
                  (casei c
                         -1 (throw (ex-info "Unexpected end of file while reading new triple" {:stack stack}))
                         \@ (throw (ex-info "Illegal prefix line in embedded object" {:line (read-n r 80) :stack stack}))
                         (\, \.) (throw (ex-info "Unexpected " c " where a single object is expected" {:line (read-n r 80) :stack stack}))
                         \_ (let [b (read-blank-node r)]
                              (emit triples subject predicate b)
                              (recur (readc r) nxt (drop offset stack)))
                         \< (let [uri (read-uri r @context)]
                              (emit triples subject predicate uri)
                              (recur (readc r) nxt (drop offset stack)))
                         \[ (let [object (new-node n)]
                              (emit triples subject predicate object)
                              (recur (readc r) :predicate-list (ccons nxt object (drop offset stack))))
                         \( (let [object (new-node n)]
                              (emit triples subject predicate object)
                              (recur (readc r) :object (ccons :collection object stack)))
                         (\" \') (let [object (read-literal r @context)]
                                   (emit triples subject predicate object)
                                   (recur (readc r) nxt (drop offset stack)))
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (let [object (read-numeric-literal r)]
                                                           (emit triples subject prediate object)
                                                           (recur (readc r) nxt (drop offset stack)))
                         (\t \f) (let [object (read-boolean-literal r c)]
                                   (emit triples subject predicate object)
                                   (recur (readc r nxt (drop offset stack))))
                         (let [object (read-local r @context)]
                           (emit triples subject predicate object)
                           (recur (readc r) nxt (drop offset stack)))))
        ;; reading a predicate-object list
        :predicate-list (casei c
                               -1 (throw (ex-info (str "Unexpected end of file after reading subject " (first stack))
                                                  {:stack stack}))
                               \[ (throw (ex-info (str "Illegal blank node in a predicate position for subject " (first stack))
                                                  {:stack stack}))
                               \. (if (> (count stack) 2)
                                    (throw (ex-info "Illegal end of statement in predicate/object sequence" {:stack stack}))
                                    (recur (readc r) :newstmt ()))
                               \; (recur (readc r) :predicate-list stack)
                               \] (let [nxt (first stack)]
                                    (recur (readc r) nxt (drop 2 stack)))
                               \< (let [uri (read-uri r @context)]
                                    (recur (readc r) :object-list (ccons :predicate-list uri stack)))
                               (let [predicate (read-local r @context)]
                                 (recur (readc r) :object-list (ccons :predicate-list predicate stack))))
        :object-list (casei c
                            -1 (throw (ex-info (str "Unexpected end of file in object list: " (first stack)) {:stack stack}))
                            \@ (throw (ex-info "Illegal prefix line in object list" {:line (read-n r 80) :stack stack}))
                            \, (recur (readc r) :object-list stack)
                            \; (recur (readc r) :predicate-list (drop 2 stack))
                            (recur (readc r) :object (cons :object-list stack))) ;; NOTE: single stack value
        :collection (casei c
                           -1 (throw (ex-info (str "Unexpected end of file in collection: " (first stack)) {:stack stack}))
                           \@ (throw (ex-info "Illegal prefix line in collection" {:line (read-n r 80) :stack stack}))
                           (\, \. \;) (throw (ex-info (str "Unexpected " c " in collection") {:line (read-n r 80) :stack stack}))
                           (let [cn (new-node n)
                                 nxt (first stack)]
                             (if (= :collection nxt)
                               (let [prev (second stack)
                                     nxt (nth stack 2)]
                                 (emit triples prev :rdf/rest cn)
                                 (recur (readc r) :object (ccons :collection :rdf/first (ccons nxt cn (drop 2 stack)))))
                               (throw "Drat. Expecting collection on the stack" {:stack stack :line (read-n r 80)}))))
        (throw (str "Unexpected state: " state) {:stack stack})))))

(defn read-ttl
  "Reads TTL from the reader. Returns a map containing triples, and a context map.
  Triples are 3 element vectors.
  A context map is a map of local-name strings to URI namespace strings."
  [^Reader r]
  (let [triples (volatile! (transient []))
        context (volatile! {})]
    (when-not (= :eof (sm-read r context triples :newstmt))
      (throw (ex-info "Finished parsing without reaching the end of the file." {:count (count @triples)})))
    {:triples (persistent! @triples)
     :context @context}))

(defn read-string
  [s]
  (with-open [r (StringReader. s)]
    (read-ttl r)))
