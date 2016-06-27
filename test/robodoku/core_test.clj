(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [robodoku.core :refer :all]))

(deftest reading-a-puzzle
  (let [p (read-puzzle "resources/puzzles/four_by_four.txt")]
    (is (= #{"A1" "A2" "A3" "A4"
             "B1" "B2" "B3" "B4"
             "C1" "C2" "C3" "C4"
             "D1" "D2" "D3" "D4"}
           (set (keys p))))

    (is (= #{3} (p "A1")))
    (is (= #{1 2 3 4} (p "D2")))))

(deftest units-and-peers
  (is (= #{"A2" "A3" "A4"} (row "A1" 4)))
  (is (= #{"A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"} (row "A1" 9)))
  (is (= #{"B1" "C1" "D1"} (col "A1" 4)))
  (is (= #{"B1" "C1" "D1" "E1" "F1" "G1" "H1" "I1"} (col "A1" 9)))
  (is (= #{"A2" "B1" "B2"} (block "A1" 4)))
  (is (= #{"A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3"} (block "A1" 9)))
  (is (= #{"A2" "A3" "A4"
           "B1" "C1" "D1"
           "B2"} (peers "A1" 4)))

  (is (= #{"A2", "B2", "D2", "E2", "F2", "G2", "H2", "I2", "C1", "C3",
           "C4", "C5", "C6", "C7", "C8", "C9", "A1", "A3", "B1", "B3"}
         (peers "C2" 9)))

  (is (= #{#{"A2" "A3" "A4"}
           #{"B1" "C1" "D1"}
           #{"A2" "B1" "B2"}}
         (units "A1" 4))))

(run-tests)
