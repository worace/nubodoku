(ns robodoku.core
  (:require [clojure.string :refer [split join]]))

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
