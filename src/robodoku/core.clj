(ns robodoku.core
  (:require [clojure.string :refer [split join]]
            [clojure.set :refer [union]]))

(defn sqrt [num] (int (Math/sqrt num)))

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
  (disj (set (for [row (take (sqrt size) alphabet)
                   col (take (sqrt size) (iterate inc 1))]
               (str row col)))
        cell))

(defn units [cell size] (set ((juxt row col block) cell size)))

(defn peers [cell size] (reduce union (units cell size)))
