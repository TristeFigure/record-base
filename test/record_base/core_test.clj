(ns record-base.core-test
  (:require [clojure.test :refer :all]
            [record-base.core :refer :all]))


(defn get-sigs [b]
  (->> b :impls
       (mapcat (fn [[proto methods]]
                 (map first methods)))))

(defmacro assert-base [b & {:keys [fields impls]}]
  `(let [[fields-pred# impls-pred#] [~fields ~impls]
         b# ~b]
     (when fields-pred#    (is (fields-pred# (base-fields b#))))
     (when impls-pred#     (is (impls-pred#  (base-impls  b#))))))

(defmacro assert-no-exception [& body]
  `(is (true? (try ~@body
                true
                (catch Throwable t#
                  false)))))

(defmacro assert-exception [pred & body]
  `(is (true? (try ~@body
                false
                (catch Throwable t#
                  (~pred t#))))))


(defprotocol P1
  (m1 [_])
  (m2 [_ x]))

(defbase BMin [])

(defrecord-from-base RMin BMin [])
(defbase BFieldsMin [a b c])
(defrecord-from-base RFieldsMin BFieldsMin [])
(defbase B1FieldsInherit [])
(defbase B2FieldsInherit [a])
(defrecord-from-base RFieldsInherit [B1FieldsInherit B2FieldsInherit] [])
(defbase B1FieldsMerge [a b])
(defbase B2FieldsMerge [a c])
(defrecord-from-base R1FieldsMerge [B1FieldsMerge B2FieldsMerge] [])
(defbase B3FieldsMerge [^long a b])
(defbase B4FieldsMerge [a #^String c])
(defrecord-from-base R2FieldsMerge [B3FieldsMerge B4FieldsMerge] [])
(defbase B1FieldsHints [^long a b])
(defbase B2FieldsHints [a #^String b])
(defrecord-from-base RFieldsHints [B1FieldsHints B2FieldsHints] [])

(defprotocol P1
  (m1 [_])
  (m2 [_ x]))

(defbase BImplsMin []
  P1 (m1 [_] :m1))
(defrecord-from-base RImplsMin BImplsMin [])
(defbase B1ImplsInherit []
  P1 (m1 [_] :from-b1))
(defbase B2ImplsInherit []
  P1 (m1 [_] :from-b2))   
(defrecord-from-base R1ImplsInherit [B1ImplsInherit B2ImplsInherit] [])
(defrecord-from-base R2ImplsInherit [B2ImplsInherit B1ImplsInherit] [])
(defrecord-from-base R3ImplsInherit [B1ImplsInherit] []
  P1 (m1 [_] :from-record))
(defrecord-from-base R4ImplsInherit B1ImplsInherit []
  P1 (m1 [_] :from-single-base-record))

(definterface I
  (^int m [#^String y]))

(defbase B1ImplsHints []
  I (^int m [_ #^String x] 1))
(defbase B2ImplsHints []
  I (^int m [_ #^String x] 2))
(defrecord-from-base R1ImplsHints [B1ImplsHints B2ImplsHints] []
  I (^int m [_ #^String x] 3))

(defbase BDeftype []
  P1 (m1 [_] :from-base))
(deftype-from-base TDeftype BDeftype []
  P1 (m1 [_] :from-type))

(defbase BExtendType []
  P1 (m1 [_] :from-base))
(defrecord RExtendType [])
(extend-type-from-base RExtendType BExtendType)


(deftest test-minimal
  (testing "asserting presence of tag protocol"
    (assert-base BMin :impls #{'{record-base.core-test/BMinP {}}}))
  (testing "testing defrecord-from-base"
    (is (->RMin))))

(deftest test-fields-min
  (testing "minimal fields support"
    (assert-base BFieldsMin :fields #(-> % keys set (= '#{a b c})))
    (testing ": asserting record"
      (is (= (.a (->RFieldsMin :a :b :c))
             :a)))))

(deftest test-fields-inheritance
  (testing "fields inheritance"
    (let [r (->RFieldsInherit :a)]
      (testing ": asserting tag protocols"
        (are [x] (satisfies? x r)
             B1FieldsInheritP
             B2FieldsInheritP))
      (testing ": asserting fields"
        (is (= (.a r) :a))))))

(deftest test-fields-merge
  (testing "fields support merging"
    (let [r (->R1FieldsMerge :a :b :c)]
      (is (= ((juxt :a :b :c) r)
             '(:a :b :c)))))
  (testing "hinted fields support merging"
    (let [merg (merge-bases B3FieldsMerge B4FieldsMerge)]
      (testing ": asserting base"
        (assert-base merg :fields #(= % '{a {:tag long}
                                          b {}
                                          c {:tag String}}))))
    (testing ": asserting record"
      (assert-no-exception (->R2FieldsMerge 1 :b "c")))
    (assert-exception #(= (class %) ClassCastException)
                      (->R2FieldsMerge :wrong :b "c"))
    (let [r (->R2FieldsMerge 1 :b "c")]
      (is (= ((juxt :a :b :c) r)
             '(1 :b "c"))))))

(deftest test-fields-hints
  (testing "fields support hints"
    (let [r (->RFieldsHints (long 123) "abc")
          merg (merge-bases B1FieldsHints B2FieldsHints)]
      (assert-base merg :fields #(= % '{a {:tag long}, b {:tag String}}))
      (is (= (.a r) 123))
      (is (= (.b r) "abc")))))


(deftest test-impls-min
  (testing "minimal impls support"
    (assert-base
      BImplsMin :impls
      #(= (mapcat (fn [[k v]]
                    (map first v))
                  %)
          '([m1 [_]])))
    (testing ": asserting record"
      (is (= (.m1 (->RImplsMin))
             :m1)))))


(deftest test-impls-inheritance
  (testing "impls inheritance"
    (testing ": base over base"
      (let [r1 (->R1ImplsInherit)
            r2 (->R2ImplsInherit)]
        (is (= (.m1 r1) :from-b2))
        (is (= (.m1 r2) :from-b1))
        (testing ": asserting tag protocols"
          (are [x] (satisfies? x r1)
               B1ImplsInheritP
               B2ImplsInheritP))))
    (testing ": record over base"
      (let [r3 (->R3ImplsInherit)]
        (testing ": asserting impls overriding"
          (is (= (.m1 r3) :from-record))))))
  (testing "a base rather than a vector of bases can be used to define a record"
    (let [r (->R4ImplsInherit)]
      (is (= (.m1 r)
             :from-single-base-record)))))

(deftest test-impls-hints
  (testing "impls support hints"
    (let [r1 (->R1ImplsHints)
          merg (merge-bases B1ImplsHints B2ImplsHints)]
      (assert-base merg :impls #(->> %
                                     (mapcat (fn [[k v]]
                                               (keys v)))
                                     (= '([m [_ _]]))))
      (is (= (.m r1 "abc") 3))
      (assert-exception #(= (class %) ClassCastException)
                        (.m r1 :wrong)))))
    
    
(deftest test-deftype-from-base
  (testing "deftype-from-base"
    (is (= (.m1 (TDeftype.))
           :from-type))))


(deftest test-extend-type-from-base
  (testing "deftype-from-base"
    (is (= (m1 (->RExtendType))
           :from-base))))

(run-tests)
