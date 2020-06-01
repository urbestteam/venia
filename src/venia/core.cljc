(ns venia.core
  (:require [venia.spec :as spec]
            [clojure.string :as str])
  #? (:clj
      (:import (clojure.lang IPersistentMap Keyword IPersistentCollection)
               (java.util UUID))))

(defprotocol ArgumentFormatter
  "Protocol responsible for query arguments' formatting to string.
  Has separate implementations for general data types in cljs and clj."
  (arg->str [arg prefix]))

(defn arguments->str
  "Given a map of query arguments, formats them and concatenates to string.

  E.g. (arguments->str {:id 1 :type \"human\"}) => id:1,type:\"human\""
  [args]
  (->> (for [[k v] args]
         [(name k) ":" (arg->str v)])
       (interpose ",")
       flatten
       (apply str)))

(defn sequential->str
  "Given something that is sequential format it to be like a JSON array."
  [arg]
  (str "[" (apply str (interpose "," (map arg->str arg))) "]"))

(defn encode-string
  "Handles escaping special characters."
  [arg]
  (str "\"" (str/escape arg {\"         "\\\""
                             \\         "\\\\"
                             \newline   "\\n"
                             \return    "\\r"
                             \tab       "\\t"
                             \formfeed  "\\f"
                             \backspace "\\b"}) "\""))

#? (:clj (extend-protocol ArgumentFormatter
           nil
           (arg->str [arg prefix] "null")
           String
           (arg->str [arg prefix] (encode-string arg))
           UUID
           (arg->str [arg prefix] (str "\"" arg "\""))
           IPersistentMap
           (arg->str [arg prefix] (str "{" (arguments->str arg prefix) "}"))
           IPersistentCollection
           (arg->str [arg prefix] (str "[" (apply str (interpose "," (map #(arg->str %1 prefix) arg))) "]"))
           Keyword
           (arg->str [arg prefix]
             (if (str/blank? prefix)
               (name arg)
               (->> (name arg) (drop 1) (apply str) (str "$" prefix "_"))))
           Object
           (arg->str [arg prefix] (str arg))))

#? (:cljs (extend-protocol ArgumentFormatter
            nil
            (arg->str [arg prefix] "null")
            string
            (arg->str [arg prefix] (encode-string arg))
            UUID
            (arg->str [arg prefix] (str "\"" arg "\""))
            PersistentArrayMap
            (arg->str [arg prefix] (str "{" (arguments->str arg prefix) "}"))
            PersistentHashMap
            (arg->str [arg prefix] (str "{" (arguments->str arg prefix) "}"))
            PersistentVector
            (arg->str [arg prefix] (sequential->str arg prefix))
            IndexedSeq
            (arg->str [arg prefix] (sequential->str arg prefix))
            LazySeq
            (arg->str [arg prefix] (sequential->str arg prefix))
            List
            (arg->str [arg prefix] (sequential->str arg prefix))
            Keyword
            (arg->str [arg prefix]
              (if (str/blank? prefix)
                (name arg)
                (->> (name arg) (drop 1) (apply str) (str "$" prefix "_"))))
            number
            (arg->str [arg prefix] (str arg))
            object
            (arg->str [arg prefix] (str arg))
            boolean
            (arg->str [arg prefix] (str arg))))

(defn meta-field->str
  "Converts namespaced meta field keyword to graphql format, e.g :meta/typename -> __typename"
  [meta-field]
  (str "__" (name meta-field)))

(defn fields->str
  "Given a spec conformed vector of query fields (and possibly nested fields),
  concatenates them to string, keeping nested structures intact."
  [fields]
  (if (keyword? fields)
    (str "..." (name fields))
    (->> (for [[type value] fields]
           (condp = type
             :venia/meta-field (meta-field->str value)
             :venia/field (name value)
             :venia/field-with-args (str (name (:venia/field value))
                                         (when (:args value)
                                           (str "(" (arguments->str (:args value)) ")")))
             :venia/field-with-data (str (when-let [alias (name (:field/alias value))]
                                           (str alias ":"))
                                         (fields->str (:field/data value)))
             :venia/nested-field (str (name (:venia/nested-field-root value))
                                      (when (:args value)
                                        (str "(" (arguments->str (:args value)) ")"))
                                      "{"
                                      (fields->str (:venia/nested-field-children value))
                                      "}")
             :venia/nested-field-arg-only (str (name (:venia/nested-field-root value))
                                               (str "(" (arguments->str (:args value)) ")"))
             :venia/fragments (str/join " " (map #(str "..." (name %)) value))
             :venia/nested-field-with-fragments (str (name (:venia/nested-field-root value))
                                                     "{"
                                                     (str/join " " (map #(str "..." (name %))
                                                                        (:venia/fragments value)))
                                                     "}")))
         (interpose ",")
         (apply str))))

;; TODO: add support for required list and matrix
(defn var-type->str
  [[def-type variable]]
  (case def-type
    :keyword
    (name variable)

    :required
    (str (var-type->str (second variable)) "!")

    :list
    (str "[" (name (first variable)) "]")))

(defn variables->str
  "Given a vector of variable maps, formats them and concatenates to string.

  E.g. (variables->str [{:variable/name \"id\" :variable/type :Int}]) => \"$id: Int\"
  E.g. (variables->str \"prefix\" [{:variable/name \"id\" :variable/type :Int}]) => \"$prefix_id: Int\""
  [variables]
  (->> (for [{var-name :variable/name
              var-type :variable/type
              var-prefix :variable/prefix
              var-default :variable/default}
             variables]
         (str "$" (when var-prefix (str var-prefix "_")) var-name
              ":" (var-type->str var-type)
              (when var-default (str "=" (arg->str var-default)))))
       (interpose ",")
       (apply str)))

(defn fragment->str
  "Given a fragment map, formats it and concatenates to string,"
  [fragment]
  (let [fields (str "{" (fields->str (:fragment/fields fragment)) "}")]
    (str "fragment "
         (name (:fragment/name fragment))
         " on "
         (name (:fragment/type fragment))
         fields)))

(defn include-fields?
  "Include fields if fields is not empty or is a keyword.
   fields could be nil or empty for operations that return a scalar."
  [fields]
  (or (keyword? fields)
      (not (empty? fields))))

(defn apply-prefix [prefix value]
  (->> value name (drop 1) (apply str) (str "$" prefix "_") keyword))

(defn reduce-prefix-to-args [prefix]
  (fn [acc [k v]]
    (assoc acc k (if-not (keyword? v) v (apply-prefix prefix v)))))

(defmulti ->query-str
  (fn [query]
    (cond (vector? query) (first query)
          (:venia/query query) :venia/query
          (:venia/query-with-data query) :venia/query-with-data
          :else :default)))

(defmethod ->query-str :venia/query-vector
  [[_ query]]
  "Given a spec conformed query vector, creates query string with query, arguments and fields."
  (str "{"
       (->> (map ->query-str query)
            (interpose ",")
            (apply str))
       "}"))

(defmethod ->query-str :venia/query-def
  [[_ query]]
  "Given a spec conformed root query map, creates a complete query string."
  (let [operation (:venia/operation query)
        operation-with-name (when operation (str (name (:operation/type operation)) " " (:operation/name operation)))
        variables (:venia/variables query)
        variables-str (when variables (str "(" (variables->str variables) ")"))
        fragments (:venia/fragments query)
        fragments-str (when fragments (str " " (->> (map fragment->str fragments)
                                                    (interpose ",")
                                                    (apply str))))]
    (str operation-with-name
         variables-str
         "{"
         (->> (map ->query-str (:venia/queries query))
              (interpose ",")
              (apply str))
         "}"
         fragments-str)))

(defmethod ->query-str :venia/query
  [query]
  "Processes a single query."
  (let [query-def (:venia/query query)
        alias (when (:query/alias query) (str (name (:query/alias query)) ":"))
        query-str (name (:query query-def))
        args (when (:args query-def) (str "(" (arguments->str (:args query-def)) ")"))
        fields (str "{" (fields->str (:fields query-def)) "}")]
    (str alias query-str args fields)))

(defmethod ->query-str :venia/queries
  [[_ query]]
  (str "{"
       (->> (map ->query-str query)
            (interpose ",")
            (apply str))
       "}"))

(defmethod ->query-str :venia/query-with-data
  [[_ query]]
  (let [args (get-in query [:query/data :args])
        data (merge (:query/data query)
                    {:args args}
                    (when (:query/prefix query)
                      {:prefix (:query/prefix query)}))
        query-str (->query-str data)
        alias (when (:query/alias query) (str (name (:query/alias query)) ":"))]
    (str alias query-str)))

(defmethod ->query-str :query/data
  [[_ query]]
  "Processes simple query."
  (let [query-str (name (:query query))
        args (when (:args query) (str "(" (arguments->str (:args query)) ")"))
        fields (when (include-fields? (:fields query)) (str "{" (fields->str (:fields query)) "}"))]
    (str query-str args fields)))

(defmethod ->query-str :default
  [query]
  "Processes a query map (with query name, args and fields)"
  (let [query-str (name (:query query))
        args (when (:args query) (str "(" (arguments->str (:args query)) ")"))
        fields (when (include-fields? (:fields query)) (str "{" (fields->str (:fields query)) "}"))]
    (str query-str args fields)))

(defn graphql-query
  "Formats clojure data structure to valid graphql query string."
  [data]
  (-> (spec/query->spec data)
      ->query-str))

(defn pack-vareables [composite]
  (fn [values]
    (reduce
     (fn [acc [prefix data]]
       (merge acc (reduce #(assoc %1 (keyword (str prefix "_" (name (first %2)))) (second %2)) {} data)))
     {}
     (zipmap (keys composite) values))))

(defn reduce-compose
  [[index
    variable-composite
    {acc-variables :venia/variables
     acc-queries :venia/queries
     acc-fragments :venia/fragments
     acc-operation :venia/operation
     :as acc}]
   {query-operation :venia/operation :as query}]
  (let [index       (inc index)
        prefix      (if query-operation
                      (str (:operation/name query-operation) "_" index)
                      (str (:operation/name acc-operation) "_" index))
        variable-composite-fn #(update %1 prefix conj (keyword (:variable/name %2)))
        variable-fn #(assoc %1 :variable/prefix prefix)
        query-fn    #(hash-map :query/data (if (:query/data %1) (:query/data %1) %1)
                               :query/alias (keyword prefix)
                               :query/prefix prefix)
        variables   (map variable-fn (get-in query [:venia/variables]))
        queries     (map query-fn (get-in query [:venia/queries]))
        fragments   (get-in query [:venia/fragments])]
    [index
     (reduce variable-composite-fn variable-composite variables)
     (cond-> acc
       variables
       (assoc :venia/variables (concat acc-variables variables))
       queries
       (assoc :venia/queries (concat acc-queries queries))
       fragments
       (assoc :venia/fragments (conj acc-fragments fragments)))]))

(defn compose [initial-query & queries]
  (let [[_ variable-composite query] (reduce reduce-compose [0 {} initial-query] queries)]
    {:query query
     :pack (pack-vareables variable-composite)}))
