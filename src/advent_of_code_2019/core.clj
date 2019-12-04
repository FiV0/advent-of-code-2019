(ns advent-of-code-2019.core
  (:require [clojure.core.match :refer [match]])
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

;; third puzzle
(defn read-input2 [file]
  (as-> file v
    (slurp v)
    (clojure.string/replace v #"\n| " {"\n" "" " " ""})
    (clojure.string/split v #",")
    (map parse-int v)))

(defn run-program
  ([input] (run-program input 0))
  ([input i]
   (case (get input i)
     1 (-> (assoc input (get input (+ i 3)) (+ (get input (get input (inc i)))
                                               (get input (get input (+ i 2)))))
           (run-program (+ i 4)))
     2 (-> (assoc input (get input (+ i 3)) (* (get input (get input (inc i)))
                                               (get input (get input (+ i 2)))))
           (run-program (+ i 4)))
     99 input
     (throw (Exception. "Should not happen!!!")))))

(defn day2-a [input]
  (let [input-v (vec input)]
    (-> input-v
        (assoc 1 12)
        (assoc 2 2)
        run-program)))

(defn day2-a-beta [input a b]
  (let [input-v (vec input)]
    (-> input-v
        (assoc 1 a)
        (assoc 2 b)
        run-program)))

(first (day2-a (read-input2 "resources/input2.txt")))

;; fourth puzzle
(defn day2-b [input]
  (loop [noun 0
         verb 0]
    (if (= (first (day2-a-beta input noun verb)) 19690720)
      (+ (* 100 noun) verb)
      (recur (if (= verb 99) (inc noun) noun)
             (if (= verb 99) 0 (inc verb))))))

(day2-b (read-input2 "resources/input2.txt"))

;; fifth puzzle
(defn read-instruction [s]
  [(nth s 0) (parse-int (subs s 1))])

(defn read-input3 [file]
  (as-> file v
    (slurp v)
    (clojure.string/split v #"\n")
    (map #(clojure.string/split % #",") v)
    (map #(map read-instruction %) v)))

(defn build-lines
  ([instructions] (build-lines instructions [0 0] '()))
  ([instructions cur res]
   (if (empty? instructions) (reverse res)
       (let [line (match (first instructions)
                         [\R n] [cur [(+ (first cur) n) (second cur)]] 
                         [\L n] [cur [(- (first cur) n) (second cur)]]  
                         [\U n] [cur [(first cur) (+ (second cur) n)]]  
                         [\D n] [cur [(first cur) (- (second cur) n)]] 
                         :else (throw (Exception. "Should not happen!!!")))]
         (build-lines (rest instructions)
                      (second line)
                      (cons line res))))))

(defn between [x1 x2 x3]
  (and (<= (min x1 x3) x2)
       (<= x2 (max x1 x3))))

(defn lines-intersect [line1 line2]
  (match [line1 line2]
         [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
         (cond
           (or (and (= x1 x2) (= x3 x4))
               (and (= y1 y2) (= y3 y4)))
           nil
           (and (= x1 x2) (= y3 y4) (between x3 x1 x4) (between y1 y3 y2))
           [x1 y3]
           (and (= y1 y2) (= x3 x4) (between x1 x3 x2) (between y3 y1 y4))
           [x3 y1]
           :else nil)))

(defn calc-intersections [lines1 lines2]
  (->>
   (for [line1 lines1
         line2 lines2]
     (lines-intersect line1 line2))
   (filter #(not (nil? %)))))

(defn day3-a [input]
  (as-> input v
    (map build-lines v)
    (calc-intersections (first v) (second v))
    (sort (fn [[x1 y1] [x2 y2]]
            (compare (+ (Math/abs x1) (Math/abs y1))
                     (+ (Math/abs x2) (Math/abs y2)))) v)))

;; (day3-a (read-input3 "resources/test3.txt"))
(day3-a (read-input3 "resources/input3.txt"))

;; six puzzle
(defn build-lines2
  ([instructions] (build-lines2 instructions [0 0] 0 '()))
  ([instructions cur total res]
   (if (empty? instructions) (reverse res)
       (let [line (match (first instructions)
                         [\R n] [cur [(+ (first cur) n) (second cur)] total]  
                         [\L n] [cur [(- (first cur) n) (second cur)] total] 
                         [\U n] [cur [(first cur) (+ (second cur) n)] total] 
                         [\D n] [cur [(first cur) (- (second cur) n)] total]
                         :else (throw (Exception. "Should not happen!!!")))]
         (build-lines2 (rest instructions)
                      (second line)
                      (+ (second (first instructions)) total)
                      (cons line res))))))

(defn lines-intersect2 [line1 line2]
  (match [line1 line2]
         [[[x1 y1] [x2 y2] dis1] [[x3 y3] [x4 y4] dis2]]
         (cond
           (or (and (= x1 x2) (= x3 x4))
               (and (= y1 y2) (= y3 y4)))
           nil
           (and (= x1 x2) (= y3 y4) (between x3 x1 x4) (between y1 y3 y2))
           (+ dis1 dis2 (Math/abs (- y1 y3)) (Math/abs (- x1 x3)))
           (and (= y1 y2) (= x3 x4) (between x1 x3 x2) (between y3 y1 y4))
           (+ dis1 dis2 (Math/abs (- x1 x3)) (Math/abs (- y1 y3)))
           :else nil)))

(defn calc-intersections2 [lines1 lines2]
  (->>
   (for [line1 lines1
         line2 lines2]
     (lines-intersect2 line1 line2))
   (filter #(not (nil? %)))))

(defn day3-b [input]
  (as-> input v
    (map build-lines2 v)
    (calc-intersections2 (first v) (second v))
    (sort v)))

(day3-b (read-input3 "resources/input3.txt"))

;; puzzle seven
(def input4 [248345 746315])

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn two-the-same [n]
  (->> n digits frequencies vals (filter #(> % 1)) empty? not))

(defn two-the-same-b [n]
  (->> n digits frequencies vals (filter #(= % 2)) empty? not))

(defn increasing [n]
  (->> n digits (partition 2 1) (map #(apply <= %)) (every? true?)))

(defn day4-a [a b]
  (->> (range a (inc b))
       (filter two-the-same)
       (filter increasing)
       count))

(day4-a (first input4) (second input4))

;; eigth puzzle
(defn day4-b [a b]
  (->> (range a (inc b))
       (filter two-the-same-b)
       (filter increasing)
       count))

(day4-b (first input4) (second input4))

