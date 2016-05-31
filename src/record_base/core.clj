(ns record-base.core
  (:require [record-base.utils :as u]
            [clojure.set :as set]))

(def ^:private parse-impls
  @#'clojure.core/parse-impls)


;; Constructor

(defn make-base [& {:keys [fields impls]}]
  (let [[fields impls] (map #(or % {})
                            [fields impls])])
  {:fields  fields
   :impls   impls})

;; Accessors

(def base-fields :fields)

(def base-impls  :impls)

(defn set-base-fields [b fields]
  (assoc b :fields fields))

(defn set-base-impls [b impls]
  (assoc b :impls impls))



(defn- base-method-signature [[name args & body]]
  [name (mapv #(if (#{'&} %) % '_)
              args)])

(defn- args-from-signature [[name args]]
  (mapv (fn [s i]
          (if (= s '&)
            '&
            (symbol (str "_" i))))
        args
        (rest (range))))

(defn- build-base-impls [impls]
  (-> impls
      (->> (map (fn [[class-or-proto-or-iface method-sources]]
                  [(u/fully-qualify class-or-proto-or-iface)
                   (->> method-sources
                        (map (juxt base-method-signature
                                   (fn [x] `(~x))))
                        (into {}))]))
           (into {}))))

(defn- proxy-method [[name args & body :as code]]
  (let [sym (gensym (str name "-"))
        qual (symbol (str *ns* "/" sym))
        sign (base-method-signature code)
        synth-args (args-from-signature sign)
        hinted-synth-args (mapv (fn [arg synth]
                                  (with-meta synth (meta arg)))
                                args
                                synth-args)
        a (remove #{'&} synth-args)
        appl (if (= a synth-args)
               `(~qual ~@a)
               `(apply ~qual ~@a))]
    {:definition         `(defn ~sym ~args
                            ~@body)
     :proxied            `(~name ~hinted-synth-args
                            ~appl)}))


(defn- parse-fields [fields]
  (->> fields
       (map (juxt #(u/without-meta %)
                  #(let [m (meta %)]
                     (or m {}))))
       (into {})))

(defmacro defbase
  "A base is a way to represent and share implementations between records.
   specs is whatever could be written in a defrecord statement body.
   It will also define a tag protocol named (str name \"P\").
   See base-record"
  [name fields & specs]
  (let [proto-name (-> (str name "P") symbol)
        _ (eval `(defprotocol ~proto-name)) ;; tag protocol
        proto-name (u/fully-qualify proto-name)
        fields-data (parse-fields fields)
        impls (parse-impls specs)
        proxies (map (fn [[class-or-proto-or-iface methods]]
                       [class-or-proto-or-iface
                        (map proxy-method methods)])
                     impls)
        proxy-defs (mapcat (fn [[_ proxs]]
                             (map :definition proxs))
                           proxies)
        proxied-impls (->> proxies
                           (map (fn [[cpi proxs]]
                                  [cpi (map :proxied proxs)]))
                           (into {}))
        impls (-> proxied-impls
                  build-base-impls
                  (assoc proto-name {}))
        base-def `(do ~@proxy-defs
                      (def ~name
                        (quote ~(make-base :fields fields-data
                                           :impls  impls))))]
    base-def))



(defn merge-bases 
  "Fusions two or more bases to create a new one where fields and specs
  are respectively merged in the style of clojure.core/merge"
  [a & [b & more]]
  (if (not b)
    a
    (let [fields (->> [a b]
                      (map base-fields)
                      (apply u/deep-merge))
          impls (->> [a b]
                     (map (comp set keys base-impls))
                     (apply set/union)
                     (map (fn [k]
                            [k (merge (get (base-impls a) k {})
                                      (get (base-impls b) k {}))]))
                     (into {}))
          base {:fields fields
                :impls impls}]
      (if (seq more)
        (apply merge-bases base more)
        base))))

(defn- synth-base-fields [base]
  (mapv (fn [[sym meta]]
          (with-meta sym meta))
        (base-fields base)))

(defn- synth-base-specs [base]
  (->> (base-impls base)
       (sort-by (comp count second)) ;; luxury
       (mapcat (fn [[class-or-proto-or-iface m]]
                 (concat [class-or-proto-or-iface]
                         (apply concat (vals m)))))))

(defn- code-from-base [base* fields specs produce-code]
  (let [parents (map #(deref (resolve %))
                     (if (coll? base*)
                       base* [base*]))
        z {:fields (parse-fields fields)
           :impls  (-> specs parse-impls build-base-impls)}
        merged (apply merge-bases (concat parents [z]))
        synth-fields (synth-base-fields merged)
        synth-specs (synth-base-specs merged)
        code (produce-code synth-fields synth-specs)]
    code))

(defmacro defrecord-from-base
  "Expands to a defrecord statement by merging the base(s) denoted by base*
  with the base represetended by specs."
  [aname base* fields & specs]
  (code-from-base base* fields specs
    (fn [fields specs]
      `(defrecord ~aname ~fields
         ~@specs))))

(defmacro deftype-from-base
  "Same thing as defrecord-from-base but for types"
  [aname base* fields & specs]
  (code-from-base base* fields specs
    (fn [fields specs]
      `(deftype ~aname ~fields
         ~@specs))))

(defmacro extend-type-from-base
  "Same thin as defrecord-from-base but via extend-type"
  [aname base* & specs]
  (code-from-base base* [] specs
    (fn [_fields specs]
      `(extend-type ~aname
         ~@specs))))

