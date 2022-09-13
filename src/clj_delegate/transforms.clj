(ns clj-delegate.transforms
  (:use clojure.pprint)
  (:require [weaving.core]
            [shuriken.core :refer [fully-qualify unqualify index-by]]
            [clj-delegate.specs   :refer [to-deftype-specs to-local-format
                                          parse-method-code]]
            [clj-delegate.reflect :refer [protocol? signature parameter-names
                                          caching-all-protocols all-methods
                                          maybe-resolve]]))

(def or|  weaving.core/or|)
(def and| weaving.core/and|)
(def not| weaving.core/not|)

(defn abstraction?|
  "Returns a method descriptor predicate that returns true if it
  matches one of the given `abstractions`."
  [& abstractions]
  (fn [m]
    (let [protocol (-> m :protocol maybe-resolve)]
      (some #(= protocol %)
            (map maybe-resolve abstractions)))))

;; TODO: do not perform the check on :protocol/:abstraction
(defn method?|
  "Returns a method descriptor predicate that return true if it
  matches one of the given method signatures.
  A method signature here means a vector like
    `[ClassInterfaceOrProto method-name args-vec]`

  The `this` initial argument is implicit and should not be stated
  in `args-vec`.

  For now, only the number of args in `args-vec` is taken into account."
  [& signatures]
  (fn [m]
    (some (fn [sign]
            (let [[protocol name args] sign]
              (and (= (maybe-resolve (:protocol m)) (maybe-resolve protocol))
                   (= (:name m) name)
                   ;; TODO: allow to filter by arg type
                   (= (-> m :params count)
                      (count args)))))
          signatures)))

;; TODO: one day we'll want to support proxy specs as well
(defn literally|
  "Returns a method descriptor transformer that will make the
  method conform to the passed method `spec`."
  [method-spec]
  (fn [m]
    (let [[name argvec & body] method-spec]
      (assoc m
        :name name
        :this (first argvec)
        :params (-> argvec rest vec)
        :body body))))

(def ^:dynamic *default-transforms*
  {(method?| '[java.lang.Object getClass []]
             '[java.lang.Object wait [a]]
             '[java.lang.Object wait [a b]]
             '[java.lang.Object notify []]
             '[java.lang.Object notifyAll []])
   (constantly false)})

(defn- compose-matchers-and-transformers [transforms]
  (->> transforms
       (map (fn [[matcher transformer]]
              #(when %
                 (if (matcher %)
                   (transformer %)
                   %))))
       (keep identity)
       (apply comp)))

(defn apply-transforms [delegate transforms methods]
  (let [try-transforming (compose-matchers-and-transformers
                           (concat *default-transforms* transforms))]
    (->> (to-local-format delegate methods)
         vals
         (map try-transforming)
         (remove (complement identity))
         (index-by signature #(last %2))
         (to-deftype-specs delegate))))
