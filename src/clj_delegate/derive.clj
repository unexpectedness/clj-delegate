(ns clj-delegate.derive
  (:require [robert.hooke :refer [add-hook remove-hook with-scope]]
            [shuriken.core :refer [silence]]
            [clj-delegate.class])
  (:import [clj_delegate.class Delegate]))

;; Tag protocol
(defprotocol Delegation)

(def ^:private delegation-interface
  (:on-interface Delegation))

(def ^:private delegate-hierarchy
  (make-hierarchy))

(defn derive-delegate [child parent]
  (alter-var-root #'delegate-hierarchy
                  derive
                  (symbol (.getName child))
                  (symbol (.getName parent))))

(defn- delegates?* [isa?-f child parent]
  (and (isa?-f child delegation-interface)
       (isa?-f delegate-hierarchy
             (symbol (.getName child))
             (symbol (.getName parent)))))

(def delegates? (partial delegates?* isa?))

(defn instance-delegates? [parent instance]
  (delegates?* (class instance) parent))

(defn- delegates-or-isa?*
  ([isa?-f child parent]
   (if (isa?-f child delegation-interface)
     (delegates?* isa?-f child parent)
     (isa?-f child parent)))
  ([isa?-f h child parent]
   (if (isa?-f child delegation-interface)
     (delegates?* isa?-f child parent)
     (isa?-f h child parent))))

(def delegates-or-isa?
  (partial delegates-or-isa?* isa?))

(defn instance-delegates-or-isa? [parent instance]
  (delegates-or-isa? (class instance) parent))

(defn isa?-delegate-hook
  ([original child parent]   (delegates-or-isa?* original child parent))
  ([original h child parent] (delegates-or-isa?* original h child parent)))

(defmacro with-delegates [& body]
  `(with-scope
     (add-hook #'isa? #'isa?-delegate-hook)
     ~@body))

(defmacro without-delegates [& body]
  `(do ~@body))
