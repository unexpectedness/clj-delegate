(ns clj-delegate.transforms-test
  (:refer-clojure :exclude [methods])
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clj-delegate.specs :refer [to-local-format to-deftype-specs]]
            [clj-delegate.transforms :refer :all]
            [clj-delegate.fixtures :refer [Protocol ProtocolA]])

  (:import [clj_delegate.fixtures Record
            Interface Interface2 Interface3 Interface4]))

(deftest test-apply-transforms
  (binding [*ns* (find-ns 'clj-delegate.transforms-test)]
    (let [methods (to-local-format
                    'Record
                    '(Protocol
                      (method [this a] :m)
                      ProtocolA
                      (method-a [this a] :a)
                      (method-a [this a b] :ab)
                      (method-b [this b] :b)
                      Interface
                      (m [this a] :iface)
                      Interface2
                      (mm [this a] :iface2)
                      Interface3
                      (mmm [this a] :iface3)
                      Interface4
                      (mmmm [this a] :iface4)
                      Object
                      (toString [this] "abc")))
          transforms {(abstraction?| Protocol)
                      (literally| '(method [this] :xyz))

                      (or| (method?| '[ProtocolA method-a [a]])
                           #(and (= (:protocol %) `ProtocolA)
                                 (= (:name %)     'method-b)))
                      #(update % :body (fn [body]
                                         `((do (~'println "ahah")
                                               ~@body))))

                      (abstraction?| Object)
                      (literally| '(toString [this] (println "it works")))

                      (abstraction?| Interface Interface2 Interface3 Interface4)
                      (constantly nil)}
          result (apply-transforms 'Record
                                   transforms
                                   methods)]
      (is (= '(java.lang.Object
               (toString [this] (println "it works"))
               clj-delegate.fixtures/ProtocolA
               (method-a [this a b] :ab)
               (method-a [this a] (do (println "ahah") :a))
               (method-b [this b] (do (println "ahah") :b))
               clj-delegate.fixtures/Protocol
               (method [this] :xyz))
             result)))))
