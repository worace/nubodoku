(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [robodoku.core :refer :all]))

(deftest test-reading-a-puzzle
  (let [p (read-puzzle "resources/puzzles/four_by_four.txt")]
    (is (= #{"A1" "A2" "A3" "A4"
             "B1" "B2" "B3" "B4"
             "C1" "C2" "C3" "C4"
             "D1" "D2" "D3" "D4"}
           (set (keys p))))

    (is (= #{3} (p "A1")))
    (is (= #{1 2 3 4} (p "D2")))
    ))

(run-tests)
