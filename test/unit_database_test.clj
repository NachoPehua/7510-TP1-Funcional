(ns unit-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]
			 [clojure.string :as str]))
			
(deftest is-query-complete?-test
  (testing "mujer(maria). should be true"
    (is (= (is-query-complete? "mujer(maria).")
           true)))

  (testing "mujer should be false"
    (is (= (is-query-complete? "mujer")
           false)))

  (testing "mujer( query should be false"
    (is (= (is-query-complete? "mujer(")
           false)))

  (testing "mujer) query should be false"
    (is (= (is-query-complete? "mujer)")
           false)))
  )
  
(deftest is-fact-test
  
  (testing "varon(juan) should be true"
    (is (= (is-fact (lazy-seq  ["varon" ["juan"]])(vector ["varon" ["juan"]]))
           true)))
  (testing "varon(juan) should be false"
    (is (= (is-fact (lazy-seq  ["varon" ["marcos"]])(vector ["varon" ["juan"]]))
           false)))
  (testing "varon(marcos) should be false"
    (is (= (is-fact (lazy-seq  ["varon" ["juan"]])(vector ["varon" ["marcos"]]))
           false)))
  (testing """ should be false"
    (is (= (is-fact (lazy-seq  ["varon" ["juan"]])(vector []))
           false)))
		    (testing "varon(marcos) should be false"
    (is (= (is-fact (lazy-seq  [])(vector ["varon" ["marcos"]]))
           false)))
)

(deftest parser-fact-test
  
  (testing "varon(juan) should be [varon [juan]]"
    (is (= (parser-fact "varon(juan)") ["varon" ["juan"]]))
           )
  (testing "padre(juan, pepe) should be [padre [juan pepe]]"
    (is (= (parser-fact "varon(juan, pepe)") ["varon" ["juan" "pepe"]]))
           )

)