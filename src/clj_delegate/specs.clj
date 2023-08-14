(ns clj-delegate.specs
  (:require [clj-delegate.reflect :refer [parameter-names signature all-methods
                                          caching-all-protocols qualify-type]]
            [clojure.main :refer [demunge]]
            [shuriken.namespace :refer [fully-qualify]]
            [shuriken.associative :refer [index-by]]
            [shuriken.sequential :refer [slice]]))

;                   deftype specs   list of clojure.lang.Method   local format
; to-local-format                               • -------------------> •
; to-local-format          • ----------------------------------------> •
; to-deftype-specs         • <---------------------------------------- •


(defn parse-method-code [indexed-methods spec]
  (let [[name params & body] spec
        [this & params] params
        meth (get indexed-methods [name (parameter-names params)])]
    {:name name
     :params (vec params)
     :this this
     :body body
     :return-type (or (some-> name meta :tag symbol fully-qualify)
                      (some-> meth :return-type symbol fully-qualify)
                      'java.lang.Object)
     :parameter-types (mapv #(or (-> %1 meta :tag)
                                 %2
                                 %3)
                            params
                            (:parameter-types meth)
                            (repeat 'java.lang.Object))}))

(defn partition-deftype-specs [specs]
  (slice symbol? specs
         :include-delimiter :left
         :include-empty true))

(defn to-local-format [delegate specs]
  (caching-all-protocols
    (if (-> specs meta :format (= :local-format))
      specs
      (with-meta
        (index-by
          signature #(last %2)
          (if (instance? clojure.reflect.Method (first specs))
            ;; convert from list of clojure.reflect.Methods to local format
            (->> specs
                 (map (fn [{:keys [name declaring-class protocol parameter-types
                                   return-type]
                            :as m}]
                        (let [params (parameter-names parameter-types)]
                          {:name name
                           :declaring-class declaring-class
                           :protocol (fully-qualify protocol)
                           :params params
                           :parameter-types parameter-types
                           :return-type return-type
                           :this 'this
                           :body `((~(symbol (str "." name))
                                      (.delegate ~'this)
                                      ~@params))}))))
            ;; convert defrecord/deftype quoted specs code to local format
            (let [am (index-by
                       (juxt :name (comp parameter-names :parameter-types))
                       #(last %2)
                       (distinct
                         (concat (when-let [d (resolve delegate)]
                                   (all-methods d))
                                 (mapcat all-methods (filter symbol? specs)))))]
              (->> specs
                   partition-deftype-specs
                   (mapcat
                     (fn [[proto-or-class & methods]]
                       (if (empty? methods)
                         [{:declaring-class delegate
                           :protocol (fully-qualify proto-or-class)
                           :no-method true}]
                         (for [m (map (partial parse-method-code am)
                                      methods)]
                           (assoc m
                             :declaring-class delegate
                             :protocol (fully-qualify proto-or-class))))))))))
        {:format :local-format}))))

(defn to-deftype-specs [delegate formatted-specs]
  ;; convert from local format to defrecord/deftype quoted specs code
  (let [delegate     (fully-qualify delegate)]
    (if (-> formatted-specs meta :format (= :deftype-specs))
      formatted-specs
      (do (with-meta
            (->> formatted-specs
                 vals
                 (group-by :protocol)
                 (mapcat
                   (fn [[proto methods]]
                     (if (-> methods first :no-method)
                       (list proto)
                       `(~proto
                          ~@(for [{:keys [name params this body return-type
                                          parameter-types] :as m}
                                  methods]
                              (concat
                               [(with-meta name {:tag (qualify-type
                                                       (:declaring-class m) return-type)})
                                (vec (cons (with-meta this {:tag delegate})
                                           (map #(with-meta %1 {:tag (qualify-type
                                                                      (:declaring-class m) %2)})
                                                params parameter-types)))]
                               body)))))))
            {:format :deftype-specs})))))

(defn merge-specs [delegate & args]
  (caching-all-protocols
    (to-deftype-specs
      delegate (apply merge (map (partial to-local-format delegate)
                                 args)))))
