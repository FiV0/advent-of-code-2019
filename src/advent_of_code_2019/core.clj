(ns advent-of-code-2019.core
  (:require [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

;; first puzzle
(defn parse-int [s]
  (java.math.BigInteger. s))

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

;; (day1-a (read-input "resources/input1.txt"))

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

;; (day1-b (read-input "resources/input1.txt"))

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

;; (first (day2-a (read-input2 "resources/input2.txt")))

;; fourth puzzle
(defn day2-b [input]
  (loop [noun 0
         verb 0]
    (if (= (first (day2-a-beta input noun verb)) 19690720)
      (+ (* 100 noun) verb)
      (recur (if (= verb 99) (inc noun) noun)
             (if (= verb 99) 0 (inc verb))))))

;; (day2-b (read-input2 "resources/input2.txt"))

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
;; (day3-a (read-input3 "resources/input3.txt"))

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

;; (day3-b (read-input3 "resources/input3.txt"))

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

;; (day4-a (first input4) (second input4))

;; eigth puzzle
(defn day4-b [a b]
  (->> (range a (inc b))
       (filter two-the-same-b)
       (filter increasing)
       count))

;; (day4-b (first input4) (second input4))
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

;; (day6-a "resources/input6.txt")
;; (day6-a "resources/test6.txt")

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

;; (day6-b "resources/test6.txt")
;; (day6-b "resources/input6.txt")

;; puzzle 13

(defn phase-combinantions []
  (->> (range 5)
       (combo/permutations)))

(defn run-program4
  [input i queue]
   (match (int-to-array (get input i))
          [_ m2 m1 1] (-> (assoc input (get input (+ i 3))
                                 (+ (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))) )
                          (run-program4 (+ i 4) queue))
          [_ m2 m1 2] (-> (assoc input (get input (+ i 3))
                                 (* (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))))
                          (run-program4 (+ i 4) queue))
          [_ _ _ 3] (-> (assoc input (get input (inc i)) (first queue))
                        (run-program4 (+ i 2) (rest queue)))
          [_ _ m1 4] (get-program-value input m1 (inc i))
          [_ m2 m1 5] (if (= (get-program-value input m1 (inc i)) 0)
                        (run-program4 input (+ i 3) queue)
                        (run-program4 input (get-program-value input m2 (+ i 2)) queue))
          [_ m2 m1 6] (if (= (get-program-value input m1 (inc i)) 0)
                        (run-program4 input (get-program-value input m2 (+ i 2)) queue)
                        (run-program4 input (+ i 3) queue))
          [_ m2 m1 7] (->
                       (assoc input (get input (+ i 3))
                              (if (< (get-program-value input m1 (inc i))
                                     (get-program-value input m2 (+ i 2))) 1 0))
                       (run-program4 (+ i 4) queue))
          [_ m2 m1 8] (->
                       (assoc input (get input (+ i 3))
                              (if (= (get-program-value input m1 (inc i))
                                     (get-program-value input m2 (+ i 2))) 1 0))
                       (run-program4 (+ i 4) queue))
          [_ _ _ 99] nil 
          ;; input
          :else (throw (Exception. "Should not happen!!!"))))

(defn add-to-phase-combination [phase item]
  (if (empty? phase)
    (list item)
    (cons (first phase) (cons item (rest phase))) ))

(defn run-amplifiers [input phase]
  (loop [p (add-to-phase-combination phase 0)]
    (println p)
    (if (= (count p) 1)
      (first p)
      (recur (->> (run-program4 input 0 p)
                  (add-to-phase-combination (-> p rest rest)))))))

(defn day7-a [input]
  (let [in (-> input read-input2 vec)
        phases (phase-combinantions)]
    (->> (map #(run-amplifiers in %) phases)
         (reduce max 0))))

;; (run-amplifiers (-> "resources/test7.txt" read-input2 vec) '(4 3 2 1 0))
;; (run-amplifiers (-> "resources/test7b.txt" read-input2 vec) '(0 1 2 3 4))
;; (run-amplifiers (-> "resources/test7c.txt" read-input2 vec) '(1 0 4 3 2))

;; (day7-a "resources/input7.txt")

;; puzzle 14
(defn phase-combinantions2 []
  (->> (range 5 10)
       (combo/permutations)))

(defn run-program5
  [input i queue]
   (match (int-to-array (get input i))
          [_ m2 m1 1] (-> (assoc input (get input (+ i 3))
                                 (+ (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))) )
                          (run-program5 (+ i 4) queue))
          [_ m2 m1 2] (-> (assoc input (get input (+ i 3))
                                 (* (get-program-value input m1 (inc i))
                                    (get-program-value input m2 (+ i 2))))
                          (run-program5 (+ i 4) queue))
          [_ _ _ 3] (-> (assoc input (get input (inc i)) (first queue))
                        (run-program5 (+ i 2) (rest queue)))
          [_ _ m1 4] [input (+ i 2) (get-program-value input m1 (inc i))]
          [_ m2 m1 5] (if (= (get-program-value input m1 (inc i)) 0)
                        (run-program5 input (+ i 3) queue)
                        (run-program5 input (get-program-value input m2 (+ i 2)) queue))
          [_ m2 m1 6] (if (= (get-program-value input m1 (inc i)) 0)
                        (run-program5 input (get-program-value input m2 (+ i 2)) queue)
                        (run-program5 input (+ i 3) queue))
          [_ m2 m1 7] (->
                       (assoc input (get input (+ i 3))
                              (if (< (get-program-value input m1 (inc i))
                                     (get-program-value input m2 (+ i 2))) 1 0))
                       (run-program5 (+ i 4) queue))
          [_ m2 m1 8] (->
                       (assoc input (get input (+ i 3))
                              (if (= (get-program-value input m1 (inc i))
                                     (get-program-value input m2 (+ i 2))) 1 0))
                       (run-program5 (+ i 4) queue))
          [_ _ _ 99] nil 
          ;; input
          :else (throw (Exception. "Should not happen!!!"))))

(defn run-amplifiers2
  ([inputs phase] (run-amplifiers2 inputs phase 0))
  ([inputs phase init]
   (loop [ins inputs
          cur init 
          i 0]
     (let [ii (mod i 5)
           [input cnt] (nth ins ii)
           res (run-program5 input cnt (if (< i 5) [(nth phase i) cur] [cur]))]
       (if (nil? res)
         cur
         (recur (assoc ins ii [(first res) (second res)])
                (nth res 2)
                (inc i)))))))

(defn day7-b [input]
  (let [in (-> input read-input2 vec)
        inputs (vec (repeat 5 [in 0]) )
        phases (phase-combinantions2)]
    (->> (map #(run-amplifiers2 inputs %) phases)
         (reduce max 0))))

;; (day7-b "resources/input7.txt")

;; puzzle 15
(defn read-input7 [file]
  (->> file
    slurp 
    seq
    (filter #(not (= % \newline)))
    (map #(parse-int (str %)))))

(def width 25)
(def height 6)
(def size (* width height))

(defn day8-a [input]
  (let [partitions (->> input (partition size))
        index (->> partitions
                   (map-indexed (fn [i p] [i (reduce #(if (= %2 0) (inc %1) %1) 0 p)]))
                   (reduce (fn [[i1 cur1] [i2 cur2]] (if (< cur2 cur1) [i2 cur2] [i1 cur1]))[-1 (inc size)])
                   first)]
    (as-> (nth partitions index) v
        (reduce (fn [[cnt1 cnt2] val]
                  (case val
                    1 [(inc cnt1) cnt2]
                    2 [cnt1 (inc cnt2)]
                    [cnt1 cnt2])) [0 0] v)
        (* (first v) (second v)))))

;; (day8-a (read-input7 "resources/input8.txt"))
;; puzzle 16
(defmulti transpose
  "Switch rows with columns."
  class)
 
(defmethod transpose clojure.lang.PersistentList
  [mtx]
  (apply map list mtx))
 
(defmethod transpose clojure.lang.PersistentVector
  [mtx]
  (apply mapv vector mtx))

(defn print-picture [input]
  (loop [in input
         i 1]
    (if (empty? in)
      nil
      (do (if (= 1 (first in)) (print "1") (print " "))
          (when (= 0 (mod i width)) (println))
          (recur (rest in) (inc i))))))

(defn day8-b [input]
  (->> input
       (partition size)
       transpose
       (map #(first (filter (fn [x] (not= x 2)) %)))
       print-picture))

;; (day8-b (read-input7 "resources/input8.txt"))

;; puzzle 17 + 18
(defn expand-vector [v l]
  (if (< l (count v))
    v
    (vec (concat v (repeat (inc (- l (count v))) 0)))))

(defn get-extend [v i]
  (-> (expand-vector v i)
      (get i)))

(defn get-program-value2 [input mode i rel]
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

(defn run-program6
  [input] 
  (loop [input input i 0 r 0]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                          (+' (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                          (*' (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r)
           [_ _ m1 3] (do (println "Enter a number:")
                          (let [in (parse-int (read-line))]
                            (recur (assoc-expand input (get input (inc i)) in
                                                 m1 r)
                                   (+ i 2) r)))
           [_ _ m1 4] (do (println "Output:" (get-program-value2 input m1 (inc i) r))
                          (recur input (+ i 2) r))
           [_ m2 m1 5] (if (= (get-program-value2 input m1 (inc i) r) 0)
                         (recur input (+ i 3) r)
                         (recur input (get-program-value2 input m2 (+ i 2) r) r))
           [_ m2 m1 6] (if (= (get-program-value2 input m1 (inc i) r) 0)
                         (recur input (get-program-value2 input m2 (+ i 2) r) r)
                         (recur input (+ i 3) r))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                       (if (= (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                               (+ i 4) r)
           [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value2 input m1 (inc i) r)))
           [_ _ _ 99] input
           :else (throw (Exception. "Should not happen!!!")))))

(defn day9-a [input]
  (->> input
       read-input2
       vec
       run-program6))

;; (day9-a "resources/test9a.txt")
;; (day9-a "resources/test9b.txt")
;; (day9-a "resources/test9c.txt")
;; (Day9-a "resources/input9.txt")

;; puzzle 19

(defn read-input-astroids [file]
  (as-> file v
    (slurp v)
    (clojure.string/split v #"\n")
    (map #(vec (seq %)) v)
    (vec v)))

(defn gcd [a b]
  (if (= b 0) a (gcd b (mod a b))))

(defn smallest-vector [[a b]]
  (let [gcd (gcd (max (Math/abs a) (Math/abs b)) (min (Math/abs a) (Math/abs b)))]
    [(/ a gcd) (/ b gcd)]))

(defn generate-vectors [n m]
  (->> (combo/cartesian-product (range (inc (- n)) n) (range (inc (- m)) m))
       (filter #(not= [0 0] %))
       (map smallest-vector)
       set
       vec))

(defn get-mat [ma [i j]]
  (get (get ma j) i))

(defn mat-size [ma]
  [(count (first ma)) (count ma)])

(defn next-coordinates [i j n m]
  (if (= (inc i) n)
    [0 (inc j)]
    [(inc i) j]))

(defn add-vectors [[i j] [x y]]
  [(+ i x) (+ j y)])

(defn check-bounds [[i j] n m]
  (and (<= 0 i) (<= 0 j) (< i n) (< j m)))

(defn find-astroid [input [i j] [x y]]
  (let [[n m] (mat-size input)]
    (loop [i (+ i x)
           j (+ j y)]
      (cond (not (check-bounds [i j] n m)) 0 
            (= (get-mat input [i j]) \#) 1
            :else (recur (+ i x) (+ j y))))))

(defn day10-a [input]
  (let [[n m] (mat-size input)
        vectors (generate-vectors n m)]
    (loop [i 0 j 0 best -1 pos [-1 -1]]
      (if (= j m) [best pos]
        (let [[nexti nextj] (next-coordinates i j n m)]
          (if (= (get-mat input [i j]) \.)
            (recur nexti nextj best pos)
            (let [res (->> (for [v vectors]
                             (find-astroid input [i j] v))
                           (apply +))
                  pos (if (< best res) [i j] pos)
                  best (if (< best res) res best)]
              (recur nexti nextj best pos))))))))

;; (read-input-astroids "resources/test10a.txt")
(def coordinates
  (second (day10-a (read-input-astroids "resources/input10.txt"))))

;; puzzle 20
(defn cross-product [[i j] [x y]]
  (- (* i y) (* x j)))

(defn less [[x1 y1] [x2 y2]]
  (cond (and (>= x1 0) (< x2 0)) true
        (and (< x1 0) (>= x2 0)) false
        (and (= x1 0) (= x2 0) (or (>= y1 0) (>= y2 0))) (> y1 y2)
        (and (= x1 0) (= x2 0)) (> y2 y1)
        :else (let [det (cross-product [x1 y1] [x2 y2])]
                (cond (< det 0) true
                      (< 0 det) false
                      :else (throw (Exception. "Should not happen!!!"))))))

(defn find-astroid2 [input [i j] [x y]]
  (let [[n m] (mat-size input)]
    (loop [i (+ i x)
           j (+ j y)]
      (cond (not (check-bounds [i j] n m)) nil 
            (= (get-mat input [i j]) \#) [i j] 
            :else (recur (+ i x) (+ j y))))))

(defn update-mat [ma [i j] val]
  (assoc ma j (assoc (get ma j) i val)))

(defn day10-b [input coordinates target]
  (let [[n m] (mat-size input)
        vectors (->> (generate-vectors n m)
                     (sort less)
                     (map #(vector (first %) (- (second %))))
                     vec)
        nv (count vectors)]
    (loop [in input i 0 cur 0]
      (let [v (get vectors i)
            res (do
                  (find-astroid2 in coordinates v) )]
        (cond (nil? res) (recur in (mod (inc i) nv) cur)
              (= (inc cur) target) res
              :else (recur (update-mat in res \.) (mod (inc i) nv) (inc cur)))))))


;; (find-astroid2 (read-input-astroids "resources/input10.txt") [30 34] [0 1])
;; (find-astroid2 (read-input-astroids "resources/test10b.txt") [11 13] [0 -1])
;; (day10-b (read-input-astroids "resources/input10.txt") coordinates 200)

;;puzzle 21

(def vectors [[1 0] [0 1] [-1 0] [0 -1]])

(defn get-correct-vec [])

(defn run-program7
  [start-res input] 
  (loop [input input i 0 r 0 cor [0 0] veccnt 0 outcnt 0 res start-res]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                          (+' (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r cor veccnt outcnt res)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                          (*' (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r cor veccnt outcnt res)
           [_ _ m1 3] (let [in (get res cor)
                            in (if (nil? in) 0 in)]
                        (recur (assoc-expand input (get input (inc i)) in
                                             m1 r)
                               (+ i 2) r cor veccnt outcnt res))
           [_ _ m1 4] (let [out (get-program-value2 input m1 (inc i) r)]
                        (match [(mod outcnt 2) out]
                               [0 _] (recur input (+ i 2) r cor veccnt (inc outcnt) (assoc res cor out))
                               [1 _] (let [new-veccnt (mod (if (= out 0) (dec veccnt) (inc veccnt)) 4)]
                                       (recur input (+ i 2) r
                                              (add-vectors cor (get vectors new-veccnt))
                                              new-veccnt (inc outcnt) res))))
           [_ m2 m1 5] (if (= (get-program-value2 input m1 (inc i) r) 0)
                         (recur input (+ i 3) r cor veccnt outcnt res)
                         (recur input (get-program-value2 input m2 (+ i 2) r) r cor veccnt outcnt res))
           [_ m2 m1 6] (if (= (get-program-value2 input m1 (inc i) r) 0)
                         (recur input (get-program-value2 input m2 (+ i 2) r) r cor veccnt outcnt res)
                         (recur input (+ i 3) r cor veccnt outcnt res))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r cor veccnt outcnt res))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                       (if (= (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                               (+ i 4) r cor veccnt outcnt res)
           [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value2 input m1 (inc i) r))
                              cor veccnt outcnt res)
           [_ _ _ 99] res 
           :else (throw (Exception. "Should not happen!!!")))))

(defn day11-a [input]
  (->> input
       read-input2
       vec
       (run-program7 {})))

;; (count (day11-a "resources/input11.txt"))

;; puzzle 22

(defn paint-spaceship [m]
  (let [coordinates (->> (into [] m)
                         (map first))
        xs (map first coordinates)
        ys (map second coordinates)
        xstart (apply min xs)
        ystart (apply min ys)
        xend (apply max xs)
        yend (apply max ys)]
    (doseq [x (range xend (dec xstart ) -1)
            y (range ystart (inc yend))]
      (case (get m [x y])
        nil (pr ".")
        0 (pr ".")
        1 (pr "#"))
      (when (= y yend) (println)))))

(defn day11-a [input]
  (->> input
       read-input2
       vec
       (run-program7 {[0 0] 1})
       paint-spaceship))

;; (day11-a "resources/input11.txt")

;; puzzle 23 + 24
(defn read-input-day-12 [file]
  (as-> file v
    (slurp v)
    (clojure.string/replace v #"<" "")
    (clojure.string/replace v #">" "")
    (clojure.string/split v #"\n")
    (map #(clojure.string/split % #",") v)
    (map #(map (fn [s] (-> (clojure.string/split s #"=") second parse-int)) %) v)
    (map vec v)))

(defn add-vectors-3d [[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(defn negate-3d-vecotr [[x y z]]
  [(- x) (- y) (- z)])

(defn velocity-change [a b]
  (cond (< a b) 1
        (> a b) -1
        :else 0))

(defn moons-update-velocity [[[x1 y1 z1] v1] [[x2 y2 z2] v2]]
  (let [velo-change [(velocity-change x1 x2)
                     (velocity-change y1 y2)
                     (velocity-change z1 z2)]
        velo1 (add-vectors-3d v1 velo-change)
        velo2 (add-vectors-3d v2 (negate-3d-vecotr velo-change))]
    [[[x1 y1 z1] velo1] [[x2 y2 z2] velo2]]))

;; (moons-update-velocity [[-1 0 2] [0 0 0]] [[2 -10 -7] [0 0 0]])

(defn apply-velocity [[coordinates velocity]]
  [(add-vectors-3d coordinates velocity) velocity])

(defn moons-simulation [moons]
  (let [moon-map (apply hash-map (interleave (range) moons))
        combis (combo/combinations (range 4) 2)]
    (->> (reduce (fn [mm [i j]]
                   (let [[m1 m2] (moons-update-velocity (get mm i) (get mm j))]
                     (-> (assoc mm i m1)
                         (assoc j m2)))) moon-map combis)
         (map (fn [[_ m]] (apply-velocity m))))))


(defn abs [x] (max x (- x)))

(defn energy [v]
  (->> (map abs v)
       (apply +)))

(defn total-energy [moons]
  (->> (map #(map energy %) moons)
       (map #(apply * %))
       (apply +)))

;; (read-input-day-12 "resources/input12.txt")

(defn simulate [moons n]
  (loop [ms moons i 0]
    (if (= i n) ms
        (recur (moons-simulation ms) (inc i)))))

(defn day12-a [input n]
  (-> (map #(vector % [0 0 0]) input)
      (simulate n)
      total-energy))

;; (day12-a (read-input-day-12 "resources/input12.txt") 1000)
;; (day12-a (read-input-day-12 "resources/test12b.txt") 100)
;; (day12-a (read-input-day-12 "resources/test12.txt") 10)

(defn dimension-velocity-update [[x v1] [y v2]]
  [[x (+ v1 (velocity-change x y))] [y (+ v2 (velocity-change y x))]])

(defn dimension-apply-velocity [[x v]]
  [(+ x v) v])

(defn simulate-one-dimension [moons]
  (let [combis (combo/combinations (range 4) 2)]
    (->> (reduce (fn [mm [i j]]
                   (let [[m1 m2] (dimension-velocity-update (get mm i) (get mm j))]
                     (-> (assoc mm i m1)
                         (assoc j m2)))) moons combis)
         (mapv (fn [m] (dimension-apply-velocity m))))))

(defn simulate2 [dimension]
  (loop [d dimension i 0 m {}]
    (if (contains? m d)
      [(get m d) i]
      (recur (simulate-one-dimension d) (inc i) (assoc m d i)))))

(defn transform-to-single-dimensions [input]
  (transpose (mapv #(apply mapv vector %) input)))

(defn lcm [a b]
  (-> (*' a b)
      (/ (gcd a b))))

(defn day12-b [input]
  (->> (mapv #(vector % [0 0 0]) input)
       transform-to-single-dimensions
       (map simulate2)
       (map second)
       (reduce lcm)))

;; (day12-b (read-input-day-12 "resources/test12b.txt"))
;; (day12-b (read-input-day-12 "resources/input12.txt"))
