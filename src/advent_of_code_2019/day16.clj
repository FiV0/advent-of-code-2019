(ns advent-of-code-2019.day16
  (:require [advent-of-code-2019.core :refer [parse-int]]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn string-to-digit-list [s]
  (map #(- (int %) (int \0)) (seq s)))

(defn read-input [file]
  (as-> (slurp file) v
    (clojure.string/split v #"\n")
    (string-to-digit-list (first v))))

(defn calc-pattern [i]
  (->> (concat (repeat i 0) (repeat i 1) (repeat i 0) (repeat i -1))
       (cycle)
       (drop 1)))

(defn last-digit [n]
  (if (< n 0)
    (mod (- n) 10)
    (mod n 10)))

(defn calc-digit [ls i]
  (->> (map *' ls (calc-pattern i))
       (reduce +')
       last-digit))

(defn calc-digits [ls]
  (map #(calc-digit ls %) (range 1 (inc (count ls)))))

(defn iterate-calc-digits [ls n]
  (loop [i 0 res ls]
    (if (= i n) res
        (recur (inc i) (calc-digits res)))))

(defn day16-a [file]
  (as-> (read-input file) v
    (iterate-calc-digits v 100)
    (take 8 v)))

;; (day16-a "resources/test16a.txt")
;; (day16-a "resources/input16.txt")

(defn gcd [a b]
  (if (= (mod a b) 0) b
      (gcd b (mod a b))))

(defn overlay [lazy len pat-repeat]
  (->> (partition len lazy)
       (take pat-repeat)
       (apply map +)))

(defn mul-seq [seq n]
  (map #(* % n) seq))

(defn calc-pattern-overlay [i len n]
  (let [seq (calc-pattern i)
        pat-len (* 4 i)
        pat-repeat (/ pat-len (gcd len pat-len))
        repeat (int (/ n pat-repeat))
        repeat-rest (mod n pat-repeat)
        normal  (mul-seq (overlay seq len pat-repeat) repeat)
        rest (overlay seq len repeat-rest)]
    (map +' normal rest)))

(defn calc-digit-overlay [ls i n]
  (->> (map *' ls (calc-pattern-overlay i (count ls) n))
       (reduce +')
       last-digit))

(defn calc-digits-overlay [ls n]
  (map #(calc-digits-overlay ls % n) (range 1 (inc (count ls)))))

(defn iterate-calc-digits-overlay [ls n seq-mul]
  (loop [i 0 res ls]
    (if (= i n) res
        (recur (inc i) (calc-digits-overlay res seq-mul)))))

(defn number-from-array [v]
  (parse-int (apply str v)))

(defn day16-a [file]
  (let [in (read-input file)
        taking (number-from-array (take 7 in))
        res (iterate-calc-digits v 100 10000)]
    (take 8 (drop ))))
