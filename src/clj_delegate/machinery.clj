(ns clj-delegate.machinery
  (:require [clojure.string :as str]
            [shuriken.namespace :refer [fully-qualify]]
            [clj-delegate.reflect
             :refer [get-basis interface? protocol?
                     native-record-interfaces]
             :as reflect]
            [clj-delegate.derive :refer [derive-delegate]]
            [clj-delegate.specs :refer [merge-specs to-local-format]]
            [weaving.core :refer :all]))

(defn protocol-symbol-to-class-symbol
  "Converts from a fully qualified symbol denoting a protocol's ivar
  to a fully qualified symbol denoting the corresponding class."
  [sym]
  (let [string (str sym)
        adapt #(-> %
                   (clojure.string/replace "/" ".")
                   (clojure.string/replace "-" "_"))]
    (symbol
      (if (re-find #"/" string)
        (adapt string)
        (-> string symbol fully-qualify str adapt)))))

(defn ensure-namespaced-symbol
  "If the given symbol has no namespace, prepend it with *ns*."
  [sym]
  (let [string (str sym)]
    (symbol
      (if (re-find #"/|\." string)
        string
        (str *ns* \/ string)))))

(defn delegate-fields-protocol-name [delegate]
  (-> delegate name
      (str/split #"\.")
      last
      (str "Fields")
      symbol))

(def emit-deftype* @#'clojure.core/emit-deftype*)

(defn emit-delegate-fields-accessors [delegate]
  (let [fields (-> delegate resolve get-basis)]
    `(~(symbol (delegate-fields-protocol-name delegate))
        ~@(map (fn [field]
                 `(~(with-meta field {}) [~'this]
                          (~(symbol (str "." field))
                             (.delegate ~'this))))
               fields))))

(defn emit-with [f name gname delegate fields delegator-specs]
  (let [generated-specs (merge-specs delegate
                                     (emit-delegate-fields-accessors delegate)
                                     delegator-specs)
        local (->> (to-local-format delegate generated-specs)
                   (filter
                    (->| val :protocol
                         (or| protocol? interface? '#{java.lang.Object}))))
        protocols (->> local
                       (map (comp :protocol val))
                       distinct
                       (map ensure-namespaced-symbol)
                       (map protocol-symbol-to-class-symbol)
                       (remove '#{java.lang.Object})
                       vec)
        methods (->> local
                     (remove (fn [[[& _] method-specs]]
                               (or (:no-method method-specs)
                                   (= 'wait (:name method-specs))))) ;; TODO: cleaner
                     (map (fn [[k v]]
                            `(~(:name v) ~(->> v :params (cons 'this) vec)
                                ~@(:body v)))))
        fields (->> fields (cons 'delegate) vec)]
    ;; To find duplicate methods, uncomment
    ; (println (->> methods
    ;              (filter #(-> % first (= 'forEach)))
    ;              (map #(-> % first meta :tag))))
    ; (clojure.pprint/pprint
    ;   (->> (map (juxt first second) methods)
    ;        (group-by identity)
    ;        (filter #(> (count (second %)) 1))))
    (f name gname
       fields protocols
       methods
       {})))

(defn update-meta [x k f & args]
  (if (-> x meta (get k))
    (with-meta x (apply update (meta x) k f args))
    x))

(defn define-delegate-fields-protocol [delegate]
  (let [fields (-> delegate resolve get-basis)]
    (eval `(defprotocol ~(delegate-fields-protocol-name delegate)
             ~@(map (fn [field]
                      `(~(with-meta field {}) [~'this]))
                    fields)))
    nil))

(defn emit-import-statement [name]
  `(import ~(symbol (-> (str *ns* "."  name)
                        (clojure.string/replace "-" "_")))))

(def build-positional-factory
  @#'clojure.core/build-positional-factory)

(defn emit-positional-factory [name fields]
  (let [fields (cons 'delegate fields)]
    (build-positional-factory name
                              (symbol (-> (str *ns* "."  name)
                                          (clojure.string/replace "-" "_")))
                              fields)))

(defn emit-map-factory [name]
  (let [fully (clojure.string/replace (str *ns* \. name)
                                      "-" "_")]
    `(defn ~(symbol (str "map->" name))
       ~(str "Factory function for class " fully ", taking a map of keywords "
             "to field values.")
       [m#]
       (~(symbol (str fully "/create"))
          (if (instance? clojure.lang.MapEquivalence m#)
            m#
            (into {} m#))))))

(defn emit-factories-declarations [name & {:keys [map-factory?]}]
  `(do (declare ~(symbol (str "->" name)))
       ~(when map-factory?
          `(declare ~(symbol (str "map->" name))))))

(defn emit-return-statement [name]
  (symbol (-> (str *ns* "."  name)
              (clojure.string/replace "-" "_"))))

(defn emit-derive-statement [delegator delegate]
  `(derive-delegate ~(-> delegator
                         ensure-namespaced-symbol
                         protocol-symbol-to-class-symbol)
                    ~delegate))
