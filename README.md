# clj-delegate

Define delegates around Clojure types, records and Java classes.

## Usage

```clojure
[clj-delegate "0.1.8"]
```


```clojure
(ns my-ns
  (:require [clj-delegate.core :refer [defdelegate]]))
```

## Example

```clojure
(defprotocol SalutationP
  (greet [this])
  (farewell [this]))

(defrecord French [a b c]
  SalutationP
  (greet [this]
    :bonjour)
  (farewell [this]
    :au-revoir))

(defdelegate ForcedEnglishFarewell French [d]
  MyProtocol
  (farewell [this]
    :goodbye))

(let [record   (French. 1 2 3)
      delegate (ForcedEnglishFarewell. record 4)]
  ;; A record delegate is a record like any other record that stores
  ;; a delegate instance and supplementary fields.
  (println delegate)
  ;; => #clj_delegate.core_test.ForcedEnglishFarewell{:a 1, :b 2, :c 3, :d 4}

  ;; The underlying instance stays accessible as the `delegate' field
  (println (.delegate delegate))
  ;; => #clj_delegate.explorations.French{:a 1, :b 2, :c 3}

  ;; Note that this special field is not accessible via a keyword lookup
  (println (:delegate delegate))
  ;; => nil

  ;; Original methods are callable
  (println (.greet delegate))
  ;; => bonjour

  ;; And can be redefined at will
  (println (.farewell delegate))
  ;; => goodbye

  ;; The same goes for fields : delegate fields are directly accessible to the
  ;; the delegate.
  (println (.a delegate))
  (println (:a delegate))
  ;; => 1

  ;; And additionnal fields can be defined as well.
  (println (.d delegate))
  (println (:d delegate))
  ;; => 4

  ;; Any modification to the delegate (with the exception of its own fields)
  ;; impacts the underlying delegate. Here is an example using 'assoc'.
  ;; The same logic follows with dissoc, conj, cons, with-meta, etc...
  (println (.delegate (assoc delegate :x :y)))
  ;; => #clj_delegate.explorations.French{:a 1, :b 2, :c 3, :x :y}

  ;; More importantly, delegates are equal to the instance they wrap.
  (println (= delegate record))
  ;; => true

  ;; And derive from it in the default isa? hierarchy
  (println (isa? ForcedEnglishFarewell French))
  ;; => true

  ;; By default all methods are forwarded to the delegate
  (println (meta (ForcedEnglishFarewell. (with-meta (French. 1 2 3)
                                         {:a :aa})
                              4)))
  ;; => {:a :aa}

  ;; Except if the method has been redefined in the body of the delegate or
  ;; if it has been subject to a transform.
  )
```

## Transforms

Alternatively a `transforms` argument can be passed to `defdelegate` as
a vector or map of matcher/transformer pairs. Place it just after the arg vector.

```clojure
(defdelegate ForcedEnglishFarewell French [field1 field2]
  [[matcher1 transformer1]
   [matcher2 transformer2]]
   (... optional body in the style of deftype))
```

Example inspired from the source. This handles smooth integration with records so that `assoc`, `get`, etc work on the merge of the
delegate and the wrapped record:

```clojure
(defdelegate ExampleDelegate WrappedRecord [arg]
  {(or| (abstraction?| java.util.Map
                       clojure.lang.IHashEq
                       clojure.lang.ILookup
                       java.io.Serializable
                       java.lang.Iterable)
        (method?| '[clojure.lang.IPersistentCollection count []]
                  '[clojure.lang.IPersistentCollection equiv []]))
   (fn [m]
     (assoc m :body
       `(~(emit-call
            m (emit-merge-with-delegate
                m (:this m))))))})
```

Other example from the tests:

```clojure
(defdelegate DelegateForTransforms RecordForTransforms []
  {(method?| '[ProtoForTransforms method-a []])
   (literally| '(method-a [_] :delegate))}
  
  ProtoForTransforms
  (method-b [this] :delegate))
```

Notes:
- You can compose the functions produced by `abtraction?|`and so on using `and|`, `or|`, `not|` and other functional combinators.
- the map `m` the transformer function work on look like this:

```clojure
{:name method-a,
 :declaring-class clj_delegate.core_test.RecordForTransforms,
 :protocol clj-delegate.core-test/ProtoForTransforms,
 :params [],
 :parameter-types [],
 :return-type java.lang.Object,
 :this this,
 :body ((.method-a (.delegate this)))}
 ```

 - For more examples about transforms check how the fusion of delegates with records is handled [in the source](https://github.com/unexpectedness/clj-delegate/blob/d3ac3d514a508ae8e7e8d3b7f4bef86d3468c861/src/clj_delegate/record.clj#L96).
