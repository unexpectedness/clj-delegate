(ns clj-delegate.class
  (:require [shady.defclass :refer [defclass]]

            [shuriken.namespace :refer [with-ns]]))

;; TODO: grab code ?
(with-ns 'shady.defclass
  (defmacro defclass
    [name [& fields] & opts+specs]
    (let [[opts specs] (->> (partition-all 2 opts+specs)
                            (split-with (comp keyword? first))
                            (map (partial apply concat)))
          opts (apply hash-map opts)
          specs (first (reduce (fn [[specs iface] form]
                                 (if (sequential? form)
                                   [(update-in specs [iface] conj form) iface]
                                   [(assoc specs form []) form]))
                               [{} name] specs))
          [package opts] [(or (:package opts) (namespace-munge *ns*))
                          (dissoc opts :package)]
          pqname (symbol (str package "." name))
          prefix (or (:prefix opts) (str "__" name "-"))
          not-ifaces (hash-set 'Object (:extends opts) name)
          impl-names (apply hash-set (keys specs))
          implements (->> (apply disj impl-names not-ifaces) (map class-name) vec)
          method-specs (get specs name [])
          method-map (->> method-specs (reduce #(assoc %1 (first %2) %2) {}))
          init-name (or (:init opts) '-init)
          [method-map opts] (if (contains? method-map init-name)
                              [(dissoc method-map init-name)
                               (assoc opts :init init-name)]
                              [method-map opts])
          methods (->> method-map vals (mapcat method-sigs) vec)
          fields (if (>= 1 (count fields)) (first fields) (vec fields))
          state (-> (or (:state opts) (str prefix 'state))
                    str (.replace \- \_) symbol)
          opts (if (or (:state opts) fields)
                 (assoc opts :state state)
                 opts)
          opts (if-let [extends (:extends opts)]
                 (assoc opts :extends (class-name extends))
                 opts)
          opts (assoc opts
                 :name (str pqname)
                 :implements implements
                 :methods methods
                 :prefix prefix)]
      (generate-class opts)
      `(let [result# (import ~pqname)]
         ~@(map (partial defn-bodies pqname prefix fields state init-name)
                (apply concat (vals specs)))
         result#))))

(require 'shady.defclass :reload)

(defclass Delegate []
  :extend Object
  :constructors {[] []})
