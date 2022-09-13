(ns clj-delegate.monkey-patches.derive
  (:require [robert.hooke :refer [add-hook]]
            [clj-delegate.derive :refer [isa?-delegate-hook]]
            [com.palletops.ns-reload :as nsdeps]))

(with-ns 'clj-delegate.derive
  (defmacro with-delegates [& body]
    `(do ~@body))

  (defmacro without-delegates [target-var key & body]
    `(with-scope
       (remove-hook #'isa? #'isa?-delegate-hook)
       ~@body))

  (add-hook #'isa? #'isa?-delegate-hook)

  (nsdeps/reload 'clj-delegate.derive))
