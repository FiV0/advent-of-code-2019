(ns advent-of-code-2019.day13
  (:require [advent-of-code-2019.core :refer [parse-int read-input2 int-to-array]]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn expand-vector [v l]
  (if (< l (count v))
    v
    (vec (concat v (repeat (inc (- l (count v))) 0)))))

(defn get-extend [v i]
  (-> (expand-vector v i)
      (get i)))

(defn get-program-value [input mode i rel]
  (case mode
    0 (get-extend input (get-extend input i)) 
    1 (get-extend input i)
    2 (get-extend input (+ rel (get-extend input i)))))

(defn assoc-expand [input pos value mode rel]
  (let [input (expand-vector input pos)]
    (case mode
      0 (assoc input pos value) 
      2 (let [input (expand-vector input (+ rel pos))]
          (assoc input (+ rel pos) value))
      (throw (Exception. "Should not happen!!!")))))

(defn run-program
  [input] 
  (loop [input input i 0 r 0 res []]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                          (+' (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r res)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                          (*' (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r res)
           [_ _ m1 3] (do (println "Enter a number:")
                          (let [in (parse-int (read-line))]
                            (recur (assoc-expand input (get input (inc i)) in
                                                 m1 r)
                                   (+ i 2) r res)))
           [_ _ m1 4] (recur input (+ i 2) r (conj res (get-program-value input m1 (inc i) r)))
           [_ m2 m1 5] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (+ i 3) r res)
                         (recur input (get-program-value input m2 (+ i 2) r) r res))
           [_ m2 m1 6] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (get-program-value input m2 (+ i 2) r) r res)
                         (recur input (+ i 3) r res))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r res))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                       (if (= (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                               (+ i 4) r res)
           [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value input m1 (inc i) r)) res)
           [_ _ _ 99] res 
           :else (throw (Exception. "Should not happen!!!")))))

(defn group-by-3 [input]
  (if (empty? input) []
      (conj (group-by-3 (drop 3 input)) (take 3 input))))

(defn day13-a [input]
  (->> input
       read-input2
       vec
       run-program
       group-by-3
       (map #(nth % 2))
       (filter #(= % 2))
       count))

;; (day13-a "resources/input13.txt")

(defn display-screen [m]
  (let [max-x (->> (keys m)
                   (map first)
                   (apply max))
        max-y (->> (keys m)
                   (map second)
                   (apply max))]
    (doseq [y (range (inc max-y))
            x (range (inc max-x))]
      (if (contains? m [x y])
        (case (get m [x y])
          0 (print ".")
          1 (print "#")
          2 (print "$")
          3 (print "_")
          4 (print "o")))
      (when (= x max-x) (println)))
    (println "Score: " (get m [-1 0]))))

(defn screen-test [input]
  (->> input
       read-input2
       vec
       run-program
       group-by-3
       (reduce (fn [m [x y v]] (assoc m [x y] v)) {})
       display-screen))

;; (screen-test "resources/input13.txt")

(defn run-program2
  [input] 
  (loop [input input i 0 r 0 cur [] m {}]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                          (+' (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r cur m)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                          (*' (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r cur m)
           [_ _ m1 3] (do
                        ;; (display-screen m) 
                        ;; (println "Enter a number:")
                        (let [in 0 ;;(parse-int (read-line))
                              ]
                          (recur (assoc-expand input (get input (inc i)) in
                                               m1 r)
                                 (+ i 2) r cur m)))
           [_ _ m1 4] (if (= (count cur) 2)
                        (recur input (+ i 2) r [] (assoc m cur (get-program-value input m1 (inc i) r)))
                        (recur input (+ i 2) r (conj cur (get-program-value input m1 (inc i) r)) m))
           [_ m2 m1 5] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (+ i 3) r cur m)
                         (recur input (get-program-value input m2 (+ i 2) r) r cur m))
           [_ m2 m1 6] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (get-program-value input m2 (+ i 2) r) r cur m)
                         (recur input (+ i 3) r cur m))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r cur m))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                       (if (= (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                               (+ i 4) r cur m)
           [_ _ m1 9] (recur input (+ i 2) (+ r (get-program-value input m1 (inc i) r)) cur m)
           [_ _ _ 99] (display-screen m) 
           :else (throw (Exception. "Should not happen!!!")))))


(defn day13-b [input]
  (as-> input v
       (read-input2 v)
       (vec v)
       (assoc v 0 2)
       (run-program2 v)))

;; (day13-b "resources/input13.txt")
;; (day13-b "resources/input13hack.txt")
