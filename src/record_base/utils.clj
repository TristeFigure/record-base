(ns record-base.utils)

(defmacro without-meta [x]
  `(with-meta ~x nil))

(defn flatten-keys* [acc ks m]
  (if (and (map? m)
           (not (empty? m)))
    (reduce into
            (map (fn [[k v]]
                   (flatten-keys* acc (conj ks k) v))
                 m))
    (assoc acc ks m)))

(defn flatten-keys [m]
  (if (empty? m)
    m
    (flatten-keys* {} [] m)))

(defn deflatten-keys [m]
  (reduce (fn [acc [ks v]]
            (update-in acc ks
                       (fn [x]
                         (if x
                           (if (every? map? [x v])
                             (merge v x)
                             x)
                           v))))
          {} m))

(defn deep-merge* [m1f & [m2 & more]]
  (if (not m2)
    m1f
    (let [m2f (flatten-keys m2)
          m1m2f (merge m1f m2f)]
      (apply deep-merge* m1m2f (or more [])))))

(defn deep-merge [m1 & more]
  (deflatten-keys (apply deep-merge*
                         (flatten-keys m1)
                         more)))
