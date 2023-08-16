(ns clj-delegate.reflect
  (:refer-clojure :exclude [methods satisfies? find-protocol-impl])
  (:require [clojure.reflect :refer [reflect]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.main :refer [demunge]]
            [weaving.core :refer [and| not|]]
            [shuriken.namespace :refer [fully-qualify unqualify]]
            [shuriken.tree :refer [tree-seq-breadth]]
            [shuriken.associative :refer [index-by]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [clojure.main :as m]))

(def native-record-interfaces
  '#{clojure.lang.IRecord
     clojure.lang.IHashEq
     clojure.lang.IObj
     clojure.lang.ILookup
     clojure.lang.IKeywordLookup
     clojure.lang.IPersistentMap
     clojure.lang.Associative
     java.util.Map
     java.io.Serializable})

(defn ensure-class [x]
  (if (class? x)
    x
    (class x)))

(defn ensure-symbol [x]
  (if (class? x)
    (symbol (.getName x))
    x))

(defn maybe-resolve [proto-class-or-symbol]
  (if (symbol? proto-class-or-symbol)
    (let [x (resolve proto-class-or-symbol)]
      (if (var? x) (deref x) x))
    proto-class-or-symbol))

(defn maybe-deref [x]
  (if (var? x)
    (deref x)
    x))

(def ^:private alphabet
  '[a b c d e f g h i j k l m n o p q r s t u v w x y z])

(defn parameter-names [parameter-types]
  (vec (take (count parameter-types)
             (for [x alphabet
                   y alphabet]
               (symbol (str x y))))))

(defn interface? [x]
  (-> (maybe-resolve x)
      clojure.reflect/reflect
      :flags
      (contains? :interface)))

(defn protocol? [symbol-or-proto]
  (let [proto (if (symbol? symbol-or-proto)
                (try
                  (-> symbol-or-proto resolve deref)
                  (catch Throwable t))
                symbol-or-proto)]
    (try
      (set/subset? #{:method-builders :method-map :on :on-interface :sigs :var}
                   (-> proto keys set))
      (catch Throwable t
        false))))

(def pref
  @#'clojure.core/pref)

(def reduce1
  @#'clojure.core/reduce1)

(def super-chain
  @#'clojure.core/super-chain)

(defn- find-protocol-impl [protocol x]
  (let [protocol (maybe-deref protocol)
        proto-interface (:on-interface protocol)
        x (ensure-class (maybe-resolve x))]
    (if (or (instance? proto-interface x)
            (contains? (ancestors x)
                       proto-interface))
      x
      (let [c x
            impl #(get (:impls protocol) %)]
        (or (impl c)
            (and c (or (first (remove nil? (map impl (butlast (super-chain c)))))
                       (when-let [t (reduce1 pref (filter impl (disj (supers c) Object)))]
                         (impl t))
                       (impl Object))))))))

;; so that satisfies? uses the version of find-protocol-impl from above
(defn- satisfies? [proto-or-sym x]
  (boolean (find-protocol-impl (maybe-resolve proto-or-sym) x)))

(defn ns-protocols [ns]
  (->> (ns-interns ns)
       (keep (fn [[k v]]
               (when (var? v) (deref v))))
       (filter protocol?)))

(def ^:dynamic *all-protocols* nil)

(defn all-protocols []
  (set (or *all-protocols* (mapcat ns-protocols (all-ns)))))

(defmacro caching-all-protocols [& body]
  `(let [body-f# (fn [] ~@body)]
     (if *all-protocols*
       (body-f#)
       (binding [*all-protocols* (all-protocols)]
         (body-f#)))))

(defn protocols [class-or-symbol]
  (filter #(satisfies? % (maybe-resolve class-or-symbol))
          (all-protocols)))

(defn get-basis [x]
  (when-let [m (try (. (ensure-class x) getMethod "getBasis" nil)
                    (catch NoSuchMethodException _
                      nil))]
    (if m
      (. m invoke nil nil)
      [])))

(defn is-record? [sym]
  (contains? (:bases (reflect (resolve sym)))
             'clojure.lang.IRecord))

(defn public-method? [method]
  (contains? (:flags method) :public))

(defn static-method? [method]
  (contains? (:flags method) :static))

(defn abstract-method? [method]
  (contains? (:flags method) :abstract))

(defn base-ancestors
  "Like ancestors, but in breadth-first order, from low to high in the
  class hierarchy."
  [proto-class-or-symbol]
  (distinct
    (rest (tree-seq-breadth
            ;; branch?
            (fn [class]
              (let [bases (and class (:bases (reflect class)))]
                (not (or (nil? bases) (empty? bases)))))
            ;; children
            (fn [class]
              (->> class reflect :bases (map resolve)))
            ;; start-point
            (let [x (maybe-resolve proto-class-or-symbol)]
              (if (protocol? x)
                (:on-interface x)
                x))))))


(defn methods [proto-class-or-symbol]
  (let [proto-or-class (maybe-resolve proto-class-or-symbol)
        protos (index-by :on (protocols proto-or-class))
        protos-dict (->> protos (map (fn [[k v]]
                                       (let [decl-c (-> v :declaring-class)]))))
        methods-to-adapt (->> (base-ancestors proto-or-class)
                              (mapcat (comp :members reflect))
                              (map (fn [m]
                                     (let [k [(:name m)
                                              (-> m :parameter-types
                                                  parameter-names)]
                                           c (:declaring-class m)]
                                       [k (or (some-> (get protos c) :var (•- (<- (-> (str (-• .ns) "/" (-• .sym))
                                                                                      symbol))))
                                              c)])))
                              (into {}))]
    (when-not proto-or-class (throw (Exception. (format "Can't resolve %s" proto-class-or-symbol))))
    (->> (if (protocol? proto-or-class)
           (:on-interface proto-or-class)
           proto-or-class)
         reflect :members
         (filter #(instance? clojure.reflect.Method %))
         (map (fn [method]
                (-> method
                    (update-in [:name] #(-> % str demunge symbol))
                    (assoc :protocol
                           (let [p (get methods-to-adapt
                                        [(:name method)
                                         (-> method :parameter-types parameter-names)]
                                        (:declaring-class method))]
                             (get protos-dict p p)))))))))

(defn all-methods [proto-class-or-symbol]
  (->> (base-ancestors proto-class-or-symbol)
       (cons proto-class-or-symbol)
       reverse
       (mapcat methods)
       (filter (and| #(instance? clojure.reflect.Method %)
                     public-method?
                     (not| static-method?)))))

(defn signature [method]
  [(:return-type  method)
   (:name method)
   (vec (interleave (:parameter-types method)
                    (-> method :params parameter-names)))])

;; TODO: copied from shuriken.reflection instead. Fix dance.core
(defn method
  "Finds a method by reflection."
  [klass nme parameter-types]
  (let [klass (if (class? klass) klass (resolve (-> klass name symbol)))
        m (-> (doto (.getDeclaredMethod klass (name nme)
                                        (into-array Class parameter-types))
                (.setAccessible true)))]
    (fn [target & args]
      (.invoke m target (to-array args)))))

(defn static-method
  "Finds a static method by reflection."
  [class name parameter-types]
  (let [m (method class name parameter-types)]
    (partial m nil)))

(def prim-class (static-method clojure.lang.Compiler 'primClass [clojure.lang.Symbol]))

(defn qualify-type [declaring-class sym]
  (-> sym (or-> (if-> resolve identity (<- nil))
                (some-> prim-class .getName)
                (->> (fully-qualify (-> declaring-class maybe-resolve
                                        .getPackage .getName demunge symbol))))))
