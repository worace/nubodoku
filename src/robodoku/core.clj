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

(defn cell-possibilities [str puzzle-size]
  (case str
    " " (set (range 1 (inc puzzle-size)))
    #{(Integer/parseInt str)}))

(defn read-puzzle [path]
  (into {} (mapcat (fn [row row-name]
                     (map (fn [cell-value row-number]
                            [(str row-name row-number)
                             (cell-possibilities (str cell-value)
                                                 (count row))])
                          row
                          (iterate inc 1)))
                    (lines path)
                    alphabet)))

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

(defn constrain [puzzle]
  (let [single-value-squares (filter (fn [[sq possibilities]]
                                       (= 1 (count possibilities)))
                                     puzzle)]
    (reduce (fn [puzzle [sq possibilities]]
              (println "sq" sq "has only" possibilities)
              (reduce (fn [puzzle peer]
                        (println "removing" possibilities "from" peer)
                        (update puzzle peer difference possibilities))
                      puzzle
                      (peers sq (size puzzle))))
            puzzle
            single-value-squares))
  ;; If any cell has only 1 value, remove that value from its peers
  ;; If any unit has only 1 candidate for a value, assign the value to that cell
  )


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
