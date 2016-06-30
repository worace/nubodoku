(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.pprint :refer [pprint]]
            [robodoku.core :refer :all]))

(deftest reading-a-puzzle
  (let [p (read-puzzle "four_by_four.txt")]
    (is (= #{"A1" "A2" "A3" "A4"
             "B1" "B2" "B3" "B4"
             "C1" "C2" "C3" "C4"
             "D1" "D2" "D3" "D4"}
           (set (keys p))))

    (is (= #{3} (p "A1")))
    (is (= #{1 2 3 4} (p "D2")))
    (is (= #{1 2 3 4} (p "D3")))))

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

  (is (= #{"A1" "A2" "A3"
           "B4" "C4" "D4"
           "B3"}
         (peers "A4" 4)))

  (is (= #{"C1" "C2" "C4"
           "A3" "B3" "D3"
           "D4"}
         (peers "C3" 4)))

  (is (= #{"A2", "B2", "D2", "E2", "F2", "G2", "H2", "I2",
           "C1", "C3", "C4", "C5", "C6", "C7", "C8", "C9",
           "A1", "A3", "B1", "B3"}
         (peers "C2" 9)))

  (is (= #{#{"A2" "A3" "A4"}
           #{"B1" "C1" "D1"}
           #{"A2" "B1" "B2"}}
         (units "A1" 4))))

(deftest units-for-a-whole-puzzle
  (let [p (read-puzzle "four_by_four.txt")]
    (is (= 4 (count (rows p))))
    (is (= 4 (count (cols p))))
    (is (= 4 (count (blocks p))))
    (is (some #(= #{"A1" "A2" "A3" "A4"} %) (rows p)))
    (is (some #(= #{"A4" "B4" "C4" "D4"} %) (cols p)))
    (is (some #(= #{"A1" "A2" "B1" "B2"} %) (blocks p)))
    (is (= 12 (count (p-units p))))))

(deftest assigning-from-candidacy
  (let [p (read-puzzle "four_by_four.txt")]
    (is (= {"D2" 3}
           (sole-candidates p #{"A2" "B2" "C2" "D2"})))
    (is (= {"D2" 3}
           (sole-candidates p #{"C1" "C2" "D1" "D2"})))
    (is (= {"D2" 3}
           (sole-candidates p #{"C1" "C2" "D1" "D2"})))
    (is (= #{3} (get (assign-sole-candidate-values p)
                     "D2")))
    (is (= #{1} (get (assign-sole-candidate-values p)
                     "D3")))
    ))

(deftest propagating-constraints-on-a-puzzle
  (is (= (read-puzzle "four_by_four_solved.txt")
         (constrain (read-puzzle "four_by_four.txt"))))
  (is (= (read-puzzle "easy_solution.txt")
         (constrain (constrain (read-puzzle "easy.txt"))))))

(deftest find-easiest-unsolved-square
  (is true)
  (is (#{"D3" "D2"}
       (easiest-square (read-puzzle "four_by_four.txt")))))

(deftest recognizing-contradictory-assignments
  )

(run-tests)
