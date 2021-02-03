# record-base

A Clojure library that brings traits support for types and records.

```clojure
[record-base "0.2.0"]
```

In this library a trait is called a *base* (sorry) and aims to complement Clojure's approach of objects by following the same philosophy protocols stick to: a record is still free of any inheritance chain and merely satisfies protocols and **grounds** bases.

While protocols decouple interfaces from records, bases decouple records from implementations – in the middle, records stand as receptacles for compositions of interfaces on the one hand and for compositions of implementations on the other hand.

However symmetric this might seem, there is no one-to-one requirement between
protocols and bases: a protocol implementation can be scattered through multiple bases and a base can implement multiple protocols, even partially.

A base can be defined with

```clojure
(require '[record-base.core :refer :all])

(defbase MyBase [field-1 #^String field-2]
  MyProtocol
  (my-method-1 [_]
    :method-1)
  (my-method-2 [_ ^long x]
    :method-2)

  MyOtherProtocol
  (my-other-method [this]
    this))
```

and can be grounded into a record with
```clojure
(defbased :record MyRecord MyBase [field-2]
  MyProtocol
  (my-method-1 [_]
    (println "overloaded")))
```

To implement multiple bases:
```clojure
(defbased :record MyRecord [MyBase1 MyBase2] [my-field]
  MyProto
  (my-method [_this) 123)
```

Bases define record fields as well as protocol methods and these definitions can be partial (some fields or methods can be left off and be defined in other bases or in the consuming record). Bases are merged from left to right, like maps with `clojure.core/merge`, including fields and methods defined in the consuming record. It is thus possible, to override fields and methods defined in bases in the record definition itself. Both methods and fields support type hints. Fields are identified by their symbol across bases. Interfaces are supported and type hints can be overriden.

```clojure
(definterface MyInterface
  (^int my-method []))

(defbase MyBase [^String field]
  (^int my-method [this]
    123))

(defbased :record MyRecord MyBase [^int field]
  (^int my-method [this]
    456))

(my-method (->MyRecord)) ;; ==> 456
```

### deftype & extend-type

It also possible to write bases for types via
```clojure
(defbased :type ...)
```

and to extend type/records using bases with
```clojure
(extend-with-base Thing Base)
;; or
(extend-with-base Thing [Base1, Base2, ...])
```

### TODO

- write `(grounds? base x)` in order to mirror `(satisfies? protocol x)`

