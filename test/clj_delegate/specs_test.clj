(ns clj-delegate.specs-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clj-delegate.specs :refer :all]
            [clj-delegate.reflect :as reflect :refer [caching-all-protocols]]
            [clj-delegate.fixtures
             :refer [TagProtocol Protocol ProtocolA ProtocolB]])
  (:import [clj_delegate.fixtures Record]))

(use-fixtures :once (fn [f]
                      (caching-all-protocols
                        (f))))

(deftest test-to-local-format
  ;; We need this so that fully-qualify finds its way through this file's
  ;; namespace. By default, when `lein test` is used, the value of *ns* is the
  ;; 'user' namespace.
  (binding [*ns* (find-ns 'clj-delegate.specs-test)]
    (testing "convert from list of clojure.reflect.Methods to local format"
      (is (= '{[java.lang.Object
                m
                [java.lang.Object aa java.lang.Object ab]]
               {:name m,
                :declaring-class clj_delegate.fixtures.Record,
                :protocol clj_delegate.fixtures.Interface,
                :params [aa ab],
                :parameter-types [java.lang.Object java.lang.Object],
                :return-type java.lang.Object,
                :this this,
                :body ((.m (.delegate this) aa ab))},
               [java.lang.Object m [java.lang.Object aa]]
               {:name m,
                :declaring-class clj_delegate.fixtures.Record,
                :protocol clj_delegate.fixtures.Interface,
                :params [aa],
                :parameter-types [java.lang.Object],
                :return-type java.lang.Object,
                :this this,
                :body ((.m (.delegate this) aa))}}
             (->> (reflect/methods Record)
                  (sort-by #(:name %))
                  reverse
                  (filter #(-> % :protocol
                               (= 'clj_delegate.fixtures.Interface)))
                  (to-local-format 'Record)))))
    (testing "convert defrecord/deftype quoted specs code to local format"
      (is (= '{[nil nil []]
               {:declaring-class Record,
                :protocol clj-delegate.fixtures/TagProtocol,
                :no-method true},
               [java.lang.Object method [java.lang.Object aa]]
               {:name method,
                :params [a],
                :this this,
                :body (:abc),
                :return-type java.lang.Object,
                :parameter-types [java.lang.Object],
                :declaring-class Record,
                :protocol clj-delegate.fixtures/Protocol}}
             (to-local-format 'Record
                              '(TagProtocol
                                Protocol
                                (method [this a] :abc))))))
    (testing "edge cases"
      (is (= (to-local-format 'Record '())
             {})))))

(deftest test-to-deftype-specs
  (is (= '(Protocol (method [this a] :abc))
         (to-deftype-specs
           'Record
           '{method {:name method
                     :params [a]
                     :this this
                     :body (:abc)
                     :declaring-class Record
                     :protocol Protocol
                     :return-type int
                     :parameter-types [int]}})))
  (testing "edge cases"
    (is (= '()
           (to-deftype-specs 'Record {})))))

(deftest test-format-round-trip
  (let [initial '(clj-delegate.fixtures/Protocol (^Object method [this ^int a]
                                                            :abc))
          loc   (partial to-local-format 'Record)
          defty (partial to-deftype-specs 'Record)]
      (testing "local -> deftype -> local -> deftype -> local"
        (is (= (-> initial
                   loc
                   defty
                   loc
                   defty)
               initial)))
      (testing "local -> local"
        (is (= (-> initial loc loc)
               (-> initial loc))))
      (testing "deftype -> deftype"
        (is (= (-> initial loc defty defty)
               (-> initial loc defty))))))

(deftest test-merge-specs
  (binding [*ns* (find-ns 'clj-delegate.specs-test)]
    (is (= '(clj-delegate.fixtures/TagProtocol
             clj-delegate.fixtures/ProtocolA
             (method-a [this a] :abc)
             (method-b [this b] :uvw)
             clj-delegate.fixtures/ProtocolB
             (method-c [this c] :xyz)
             (method-c [this c x] :cx)
             java.lang.Object
             (toString [this] "str"))
           (merge-specs
             'Record
             (to-local-format 'Record '(TagProtocol
                                        ProtocolA
                                        (method-a [this a]   :abc)
                                        (method-b [this b]   :def)))
             (to-local-format 'Record '(ProtocolA
                                        (method-b [this b]   :uvw)
                                        ProtocolB
                                        (method-c [this c]   :xyz)
                                        (method-c [this c x] :cx)))
             '(Object (toString [this] "str")))))))
