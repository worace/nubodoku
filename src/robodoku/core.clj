(ns robodoku.core
  (:require [clojure.string :refer [split join]]
            [clojure.set :refer [union difference]]))

(defn sqrt [num] (int (Math/sqrt num)))

(defn col-number [square] (Integer/parseInt (str (second square))))

(defn col-offset [square puzzle-size]
  (* (sqrt puzzle-size)
     (int (/ (dec (col-number square))
             (sqrt puzzle-size)))))

(defn row-number [square] (- (int (first square)) 65))

(defn row-offset [square puzzle-size]
  (* (sqrt puzzle-size)
     (int (/ (row-number square)
             (sqrt puzzle-size)))))

(def alphabet (map (comp str char) (range 65 91)))

(defn lines [file] (-> file slurp (split #"\n")))
(defn square-values [puzzle-size] (set (range 1 (inc puzzle-size))))

(defn cell-possibilities [str puzzle-size]
  (case str
    " " (square-values puzzle-size)
    #{(Integer/parseInt str)}))

(defn read-puzzle
  ([filename] (read-puzzle filename "resources/puzzles"))
  ([filename dir]
   (into {} (mapcat (fn [row row-name]
                      (map (fn [cell-value row-number]
                             [(str row-name row-number)
                              (cell-possibilities (str cell-value)
                                                  (count row))])
                           row
                           (iterate inc 1)))
                    (lines (str dir "/" filename))
                    alphabet))))

(defn row [cell size]
  (disj (set (map (partial str (first cell))
                  (take size (iterate inc 1))))
        cell))

(defn col [cell size]
  (disj (set (map #(str % (last cell))
                  (take size alphabet)))
        cell))

(defn block [cell size]
  (disj (set (for [row (take (sqrt size)
                             (drop (row-offset cell size)
                                   alphabet))
                   col (take (sqrt size)
                             (drop (col-offset cell size)
                                   (iterate inc 1)))]
               (str row col)))
        cell))

(defn units [cell size] (set ((juxt row col block) cell size)))

(defn peers [cell size] (reduce union (units cell size)))

(defn size [puzzle] (sqrt (count puzzle)))

(defn rows [puzzle]
  (for [r (take (size puzzle) alphabet)]
    (set (map str
              (repeat r)
              (range 1 (inc (size puzzle)))))))

(defn cols [puzzle]
  (for [c (range 1 (inc (size puzzle)))]
    (set (map str
              (take (size puzzle) alphabet)
              (repeat c)))))

(defn blocks [puzzle]
  (for [block-rows (partition (sqrt (size puzzle))
                              (take (size puzzle) alphabet))
        block-cols (partition (sqrt (size puzzle))
                              (range 1 (inc (size puzzle))))]
    (set (for [r block-rows c block-cols] (str r c)))))

(defn p-units [puzzle] (reduce union ((juxt rows cols blocks) puzzle)))

(defn solved-squares [puzzle]
  (filter (fn [[sq possibilities]]
            (= 1 (count possibilities)))
          puzzle))

(defn eliminate-fixed-values-from-peers [puzzle]
  ;; If any cell has only 1 value, remove that value from its peers
  (reduce (fn [puzzle [sq possibilities]]
            (reduce (fn [puzzle peer]
                      (update puzzle peer difference possibilities))
                    puzzle
                    (peers sq (size puzzle))))
          puzzle
          (solved-squares puzzle)))

(defn sole-candidate [puzzle unit value]
  (let [candidates (filter (fn [sq] (contains? (puzzle sq) value))
                           unit)]
    (if (= 1 (count candidates))
      (first candidates)
      nil)))

(defn sole-candidates [puzzle unit]
  (apply hash-map (mapcat (fn [unit value]
                            (if-let [square (sole-candidate puzzle unit value)]
                              [square value]
                              []))
                          (repeat unit)
                          (square-values (size puzzle)))))

(defn assign-sole-candidate-values [puzzle]
  ;; If any unit has only 1 candidate square for a value,
  ;; assign the value to that square
  (->> (p-units puzzle)
       (map (partial sole-candidates puzzle))
       (reduce merge)
       (reduce (fn [puzzle [square value]]
                 (assoc puzzle square #{value}))
               puzzle)
       ))

(defn constrain [puzzle]
  (-> puzzle
      eliminate-fixed-values-from-peers
      assign-sole-candidate-values)
  )

(defn easiest-square [puzzle]
  (->> puzzle
       (sort-by (comp count last))
       (drop-while #(= 1 (count (last %))))
       (first)
       (first)))

(defn known-values [puzzle unit]
  (->> unit
       (map puzzle)
       (filter #(= 1 (count %)))
       (apply concat)))

(defn unit-contradicts?
  "Of the 'determined' squares (those with only a single remaining
   possibility), do any of them overlap?"
  [puzzle unit]
  (->> unit
       (known-values puzzle)
       (frequencies)
       (some (fn [[value frequency]] (> frequency 1)))))

(defn contradictory? [puzzle]
  (->> puzzle
       p-units
       (some (partial unit-contradicts? puzzle))))

(defn solved? [puzzle]
  (->> puzzle
       (p-units)
       (map (partial known-values puzzle))
       (map sort)
       (every? (fn [u-values] (= u-values
                                 (sort (square-values (size puzzle))))))))

(defn search [puzzle]
  (cond
    (solved? puzzle) puzzle
    (contradictory? puzzle) nil
    :else (let [ez-sq (easiest-square puzzle)
                possibilities (puzzle ez-sq)]
            (some (fn [value]
                    (-> puzzle
                        (assoc ez-sq #{value})
                        (constrain)
                        (search)))
                  possibilities))))

(defn solve [puzzle]
  (-> puzzle
      constrain
      search))

;; 4x4
;; A1 A2 A3 A4
;; B1 B2 B3 B4
;; C1 C2 C3 C4
;; D1 D2 D3 D4


;; 9x9
;; A1 A2 A3 A4 A5 A6 A7 A8 A9
;; B1 B2 B3 B4 B5 B6 B7 B8 B9
;; C1 C2 C3 C4 C5 C6 C7 C8 C9
;; D1 D2 D3 D4 D5 D6 D7 D8 D9
;; E1 E2 E3 E4 E5 E6 E7 E8 E9
;; F1 F2 F3 F4 F5 F6 F7 F8 F9
;; G1 G2 G3 G4 G5 G6 G7 G8 G9
;; H1 H2 H3 H4 H5 H6 H7 H8 H9
;; I1 I2 I3 I4 I5 I6 I7 I8 I9
