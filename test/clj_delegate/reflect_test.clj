(ns clj-delegate.reflect-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clj-delegate.reflect :refer :all]))

(use-fixtures :once (fn [f]
                      (caching-all-protocols
                        (f))))

(defprotocol Prot
  (metxxx [this]))

(defrecord Rec []
  Prot
  (metxxx [this] :yes))

(deftype Typ []
  Prot
  (metxxx [this] :no))

(def expected-ancestors
  (list
    java.lang.Object
    java.util.Map
    clojure.lang.ILookup
    clojure.lang.IHashEq
    clojure.lang.IObj
    clojure.lang.IKeywordLookup
    clojure.lang.IPersistentMap
    java.io.Serializable
    clj_delegate.reflect_test.Prot
    clojure.lang.IRecord
    clojure.lang.IMeta
    clojure.lang.Associative
    clojure.lang.Counted
    java.lang.Iterable
    clojure.lang.IPersistentCollection
    clojure.lang.Seqable))

(deftest test-base-ancestors
  (caching-all-protocols
    (testing "like ancestors ..."
        (is (= (sort-by (memfn getName) (base-ancestors Rec))
               (sort-by (memfn getName) (ancestors Rec)))))
    (testing "... but in breadth-first order"
        (is (= (base-ancestors Rec)
               expected-ancestors)))
    (testing "it does not contains the object's class"
        (is (false? (contains? (set expected-ancestors)
                               Rec))))))

(deftest test-all-methods
  (caching-all-protocols
    (testing "of a protocol"
      (is (contains? (set (map :name (all-methods Prot)))
                     'metxxx)))))

(deftest test-protocols
  (binding [*ns* (find-ns 'clj-delegate.reflect-test)]
    (are [x] (= (-> x protocols first :on)
                'clj_delegate.reflect_test.Prot)
         Rec
         'Rec)))
