(ns advent-of-code-2019.core
  (:gen-class))

;; first puzzle
(defn parse-int [s]
  (Integer/parseInt s))

(defn fuel-consumption [i]
  (-> i (/ 3) int (- 2)))

(defn read-input [file]
  (as-> file v
       (slurp v)
       (clojure.string/split v #"\n")
       (map parse-int v)))

(defn day1-a [inputs]
  (->> inputs
       (map fuel-consumption)
       (reduce +)))

(day1-a (read-input "resources/input1.txt"))

;; second puzzle
(defn fuel-consumption2 [i]
  (loop [cur i
         res 0]
    (let [ncur (max (-> cur (/ 3) int (- 2)) 0)]
      (if (= ncur 0)
        res
        (recur ncur 
               (+ res ncur))))))
  
(defn day1-b [inputs]
  (->> inputs
       (map fuel-consumption2)
       (reduce +)))

(day1-b (read-input "resources/input1.txt"))
