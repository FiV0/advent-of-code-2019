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
                                        ;
;; ninth puzzle
(defn int-to-array [i]
  [(int (/ i 10000))
   (mod (int (/ i 1000)) 10)
   (mod (int (/ i 100)) 10)
   (mod i 100)])

(defn get-program-value [input mode i]
  (if (= mode 0)
    (get input (get input i)) 
    (get input i) ))

(defn run-program2
  ([input] (run-program2 input 0))
  ([input i]
   (match (int-to-array (get input i))
          [_ m2 m1 1] (-> (assoc input (get input (+ i 3))
                                 (+ (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))) )
                           (run-program2 (+ i 4)))
          [_ m2 m1 2] (-> (assoc input (get input (+ i 3))
                                 (* (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))))
                          (run-program2 (+ i 4)))
          [_ _ _ 3] (do (println "Enter a number:")
                        (let [in (parse-int (read-line))]
                          (-> (assoc input (get input (inc i)) in)
                              (run-program2 (+ i 2)))))
          [_ _ m1 4] (do (println "Output:" (get-program-value input m1 (inc i)))
                         (run-program2 input (+ i 2)))
          [_ _ _ 99] input
          :else (prn (get input i)))))

(defn day5-a [input]
  (->> input
       read-input2
       vec
       run-program2
       ))

;; (day5-a "resources/input5.txt")

;; puzzle ten

(defn run-program3
  ([input] (run-program3 input 0))
  ([input i]
   (match (int-to-array (get input i))
          [_ m2 m1 1] (-> (assoc input (get input (+ i 3))
                                 (+ (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))) )
                           (run-program3 (+ i 4)))
          [_ m2 m1 2] (-> (assoc input (get input (+ i 3))
                                 (* (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))))
                          (run-program3 (+ i 4)))
          [_ _ _ 3] (do (println "Enter a number:")
                        (let [in (parse-int (read-line))]
                          (-> (assoc input (get input (inc i)) in)
                              (run-program3 (+ i 2)))))
          [_ _ m1 4] (do (println "Output:" (get-program-value input m1 (inc i)))
                         (run-program3 input (+ i 2)))
          [_ m2 m1 5] (if (= (get-program-value input m1 (inc i)) 0)
                        (run-program3 input (+ i 3))
                        (run-program3 input (get-program-value input m2 (+ i 2))))
          [_ m2 m1 6] (if (= (get-program-value input m1 (inc i)) 0)
                        (run-program3 input (get-program-value input m2 (+ i 2)))
                        (run-program3 input (+ i 3)))
          [_ m2 m1 7] (->
                       (assoc input (get input (+ i 3))
                              (if (< (get-program-value input m1 (inc i))
                                     (get-program-value input m2 (+ i 2))) 1 0))
                       (run-program3 (+ i 4)))
          [_ m2 m1 8] (->
                       (assoc input (get input (+ i 3))
                              (if (= (get-program-value input m1 (inc i))
                                     (get-program-value input m2 (+ i 2))) 1 0))
                       (run-program3 (+ i 4)))
          [_ _ _ 99] input
          :else (throw (Exception. "Should not happen!!!"))
          )))

(defn day5-b [input]
  (->> input
       read-input2
       vec
       run-program3
       ))

;; (def jump-test1 [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9])
;; (def jump-test2 [3 3 1105 -1 9 1101 0 0 12 4 12 99 1])
;; (def compare-test1 [3 9 8 9 10 9 4 9 99 -1 8])
;; (run-program3 compare-test1)
;; (day5-b "resources/test5.txt")
;; (day5-b "resources/input5.txt")

;;puzzle 11

(defn read-input3 [file]
  (as-> file v
    (slurp v)
    (clojure.string/split v #"\n")
    (map #(clojure.string/split % #"\)") v)))

(defn dfs [s m]
  (let [children (map #(dfs % m) (get m s))
        children-count (->> (map first children) (reduce +'))
        children-res (->> (map second children) (reduce +'))]
    [(inc children-count) (+' children-res children-count)]))

;; (dfs 1 {1 [2 3] 2 [4 5] 3 [6]} )

(defn day6-a [input]
  (->> input
       read-input3
       (reduce (fn [m [a b]]
                 (update m a #(if (nil? %) [b] (conj % b)))) {})
       (dfs "COM")
       ))

;; (day5-a "resources/input6.txt")
;; (day5-a "resources/test6.txt")

;;puzzle 12
(defn dfs2
  ([cur end m] (dfs2 cur end m nil)) 
  ([cur end m parent]
   (if (= cur end) 0
       (let [res (->> (remove #(= parent %) (get m cur))
                      (map #(dfs2 % end m cur))
                      (filter #(<= 0 %))
                      first)]
         (if (nil? res) -1
             (inc res))))))

(defn day6-b [input]
  (->> input
       read-input3
       (reduce (fn [m [a b]]
                 (-> (update m a #(if (nil? %) [b] (conj % b)))
                     (update b #(if (nil? %) [a] (conj % a))))) {})
       (dfs2 "YOU" "SAN")))

;; (day5-b "resources/test6.txt")
;; (day5-b "resources/input6.txt")
