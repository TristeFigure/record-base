# record-base

```clojure
[record-base "0.1.1"]
```

A Clojure library that brings traits support for types and records.

In this library a trait is called a *base* (sorry) and aims to complement Clojure's approach of objects by following the same philosophy protocols stick to : a record is still free of any inheritance chain and merely satisfies protocols or *grounds* bases. While protocols decouple interfaces from implementations, bases decouple implementations from interfaces – in the middle, records stand as receptacles for compositions of interfaces on the one hand and for compositions of implementations on the other hand.

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
(defrecord-from-base MyRecord MyBase [])
```

Like protocols, composition of bases happens in records. It is thus possible to ground multiple bases into the same record by specifying a vector of bases rather than a single base.
```clojure
(defrecord-from-base MyRecord [MyBase1 MyBase2] [])
```

Bases define record fields as well as protocol methods and these definitions can be partial (some fields or methods can be left off and be defined in other bases or in the final record). Bases are merged from left to right, like maps with `clojure.core/merge`, starting from the first base in the definition up to the fields and methods defined in the record. It is thus possible, to override fields and methods defined in bases in the record definition itself. Both methods and fields support type hints. Fields are identified by their symbol across bases. Interfaces are supported and type hints can be overriden (don't ask me why, I'm too crazy to know).

```clojure
(definterface MyInterface
  (^int my-method []))

(defbase MyBase [^String field]
  (^int my-method [this]
    123))

(derecord MyRecord MyBase [^int field]
  (^int my-method [this]
    456))

(my-method (->MyRecord)) ;; ==> 456
```

## Bonus

It also possible to write bases for types via
- `deftype-from-base`
- and `extend-type-from-base`

## TODO

- write `(grounds? base x)` in order to mirror `(satisfies? protocol x)`
- do something about type hints overloading

## Caution

Very alpha. Function names and arguments **will** change.

