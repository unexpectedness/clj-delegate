(ns clj-delegate.core
  (:require [clj-delegate.reflect :refer [is-record? caching-all-protocols]]
            [clj-delegate.type   :refer [emit-deftype-delegate]]
            [clj-delegate.record :refer [emit-defrecord-delegate]]
            [clj-delegate.derive]
            [clj-delegate.transforms]
            [potemkin :refer [import-vars]]))

(import-vars
  [clj-delegate.derive delegates?])

(import-vars
 [clj-delegate.transforms abstraction?| method?| literally| and| or| ->|])

(defmacro defdelegate [name delegate fields & [maybe-transforms & more]]
  (let [more (if more more [])
        [transforms delegator-specs] (if (or (vector? maybe-transforms)
                                             (map?    maybe-transforms))
                                       [maybe-transforms more]
                                       [[]
                                        (if (nil? maybe-transforms)
                                          more
                                          (cons maybe-transforms more))])]
    `(caching-all-protocols
       ~(if (is-record? delegate)
          (emit-defrecord-delegate
            name delegate fields transforms delegator-specs)
          (emit-deftype-delegate ;; Will handle delegation to java classes too.
            name delegate fields transforms delegator-specs)))))
