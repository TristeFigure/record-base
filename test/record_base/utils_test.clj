(ns record-base.utils-test
  (:require [clojure.test :refer :all]
            [record-base.utils :refer :all]
            [record-base.virtual-test-namespace :refer [a-var] :as virtual])
  (:import [record_base.virtual_test_namespace AType]))

(defn sym+ [sym]
  (str *ns* "/" sym))

(deftest test-fully-qualify
  (let [local-var :local]
    (binding [*ns* (find-ns 'record-base.utils-test)]
      (are [sym expected] (= (fully-qualify *ns* sym) expected)
           'Object              'java.lang.Object
           'fully-qualify       'record-base.utils/fully-qualify
           'local-var           'local-var
           'a-var               'record-base.virtual-test-namespace/a-var
           'virtual/another-var 'record-base.virtual-test-namespace/another-var
           'AType               'record_base.virtual_test_namespace.AType))))

(defmacro assert-flatten-roundtrip [m]
  `(let [m# ~m]
     (is (= (deflatten-keys (flatten-keys m#))
            m#))))

(deftest test-without-meta
  (is (nil? (-> [1 2 3]
                (with-meta {:x "x"})
                without-meta
                meta))))

(def m
  {:a {:b {:c 123
           :d 456}}
   :e {:f 789}})

(def flat-m
  {[:a :b :c] 123
   [:a :b :d] 456
   [:e :f]    789})

(deftest test-flatten-keys
  (testing "flattening"
    (is (= (flatten-keys m)
           flat-m))
    (testing "of empty maps"
      (assert-flatten-roundtrip {}))
    (testing "of maps nesting empty maps"
      (assert-flatten-roundtrip {:a {}}))))

(deftest test-deflatten-keys
  (is (= (deflatten-keys flat-m)
         m)))

(deftest test-deep-merge
  (testing "deep-merge"
    (is (= (deep-merge m {:a {:b {:c :xyz}}})
           {:a {:b {:c :xyz
                    :d 456}}
            :e {:f 789}}))
    (testing "of three maps"
      (is (= (deep-merge {:a :aa}
                         {:a :aaa :b :bb}
                         {:b :bbb :c :cc})
             {:a :aaa
              :b :bbb
              :c :cc})))
    (testing "overriding"
      (is (= (deep-merge {:a :a  :b :b :c :c}
                         {:a :aa :b :bb}
                         {:a :aaa}
                         {})
             {:a :aaa :b :bb :c :c})))
    (testing "overriding of nested maps"
      (is (= (deep-merge {:x {:a :a  :b :b :c :c}}
                         {:x {:a :aa :b :bb}}
                         {:x {:a :aaa}})
             {:x {:a :aaa :b :bb :c :c}})))
    (testing "overriding of empty nested maps"
      (is (= (deep-merge {:x 123}
                         {:x {}})
             {:x {}}))
      (is (= (deep-merge {:x {:y :z}}
                         {:x {:a :b}})
             {:x {:y :z :a :b}}))
      (is (= (deep-merge {:x {:y :z}}
                         {:x {}})
             {:x {:y :z}})))))

(run-tests)
