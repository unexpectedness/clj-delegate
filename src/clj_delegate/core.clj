(ns clj-delegate.core
  (:use clojure.pprint)
  (:require [clj-delegate.reflect :refer [is-record? caching-all-protocols]]
            [clj-delegate.type   :refer [emit-deftype-delegate]]
            [clj-delegate.record :refer [emit-defrecord-delegate]]
            [clj-delegate.derive]
            [potemkin :refer [import-vars]]))

(import-vars
  [clj-delegate.derive
   delegates?
   instance-delegates?
   delegates-or-isa?
   instance-delegates-or-isa?
   with-delegates
   without-delegates])

(defmacro defdelegate [name delegate fields & [maybe-vec & more]]
  (let [more (if more more [])
        [transforms delegator-specs] (if (vector? maybe-vec)
                                       [maybe-vec more]
                                       [[]
                                        (if (nil? maybe-vec)
                                          more
                                          (cons maybe-vec more))])]
    `(caching-all-protocols
       ~(if (is-record? delegate)
          (emit-defrecord-delegate
            name delegate fields transforms delegator-specs)
          (emit-deftype-delegate
            name delegate fields transforms delegator-specs)))))
