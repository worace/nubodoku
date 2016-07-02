(ns robodoku.core
  (:require [clojure.string :refer [split join]]
            [clojure.java.io :as io]
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

(defn units-containing [puzzle square]
  (->> (p-units puzzle)
       (filter (partial some #{square}))
       (set)))

(defn alt-peers [puzzle square]
  (disj (reduce union
                (units-containing puzzle square))
        square))

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
               puzzle)))

(defn constrain [puzzle]
  (-> puzzle
      eliminate-fixed-values-from-peers
      assign-sole-candidate-values))

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

(defn row-string [puzzle width row]
  (->> row
       (sort)
       (map puzzle)
       (map (partial apply str))
       (map #(format (str "%" width "s") %))
       (partition 3)
       (map #(join " " %))
       (join " | ")))

(defn display [puzzle]
  (let [width (apply max (map count (vals puzzle)))
        line-sep (str "\n"
                      (apply str (take (+ 3
                                          (* 9 (inc width)))
                                       (repeat "-")))
                      "\n")]
    (->> puzzle
         rows
         (map (partial row-string puzzle width))
         (partition (sqrt (size puzzle)))
         (map #(join "\n" %))
         (join (str line-sep))
         )))

(defn search [puzzle]
  (cond
    (contradictory? puzzle) nil
    (solved? puzzle) puzzle
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

(defn solve-bunch-of-puzzles []
  (let [puzzle-dir "resources/examples-with-solutions/puzzles"
        solution-dir "resources/examples-with-solutions/solutions"
        puzzles (->> puzzle-dir
                     (io/file)
                     (file-seq)
                     (drop 1)
                     (map #(.getName %)))]
    (doseq [fname puzzles]
      (println "Attempting puzzle" fname "...")
      (let [puzzle (read-puzzle fname puzzle-dir)
            solved (solve puzzle)
            solution (read-puzzle fname solution-dir)]
        (println (display solved))
        (assert (= solution solved))))))

(defn -main [& args] (solve-bunch-of-puzzles))
