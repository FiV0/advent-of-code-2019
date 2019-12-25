(ns advent-of-code-2019.day19
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

(defn negate [b] (if b false true))

(defn run-program6
  [input pos] 
  (loop [input input i 0 r 0 cur true]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                             (+' (get-program-value2 input m1 (inc i) r)
                                                 (get-program-value2 input m2 (+ i 2) r))
                                             m3 r)
                               (+ i 4) r cur)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                             (*' (get-program-value2 input m1 (inc i) r)
                                                 (get-program-value2 input m2 (+ i 2) r))
                                             m3 r)
                               (+ i 4) r cur)
           [_ _ m1 3] (let [in (if cur (first pos) (second pos))]
                        (recur (assoc-expand input (get input (inc i)) in m1 r)
                               (+ i 2) r (negate cur)))
           [_ _ m1 4] (get-program-value2 input m1 (inc i) r)
           [_ m2 m1 5] (if (= (get-program-value2 input m1 (inc i) r) 0)
                         (recur input (+ i 3) r cur)
                         (recur input (get-program-value2 input m2 (+ i 2) r) r cur))
           [_ m2 m1 6] (if (= (get-program-value2 input m1 (inc i) r) 0)
                         (recur input (get-program-value2 input m2 (+ i 2) r) r cur)
                         (recur input (+ i 3) r cur))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value2 input m1 (inc i) r)
                                              (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r cur))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                             (if (= (get-program-value2 input m1 (inc i) r)
                                                    (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                             m3 r)
                               (+ i 4) r cur)
           [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value2 input m1 (inc i) r)) cur)
           [_ _ _ 99] input
           :else (throw (Exception. "Should not happen!!!")))))

(defn day19-a [input]
  (let [input (->> input
                   read-input2
                   vec)]
    (->> (for [x (range 50) y (range 50)] [x y])
         (map #(run-program6 input %))
         (reduce +))))

;; (day19-a "resources/input19.txt")

(defn create-vec [at-position]
  (loop [xl 3 xr 4 y 2 res []]
    (let [x (int (/ (+ xl xr) 2))
          nxl (->> (drop-while #(= (at-position [(+ xl %) (inc y)]) 0) (range 100000))
                   first
                   (+ xl))
          nxr (->> (drop-while #(= (at-position [(+ (inc xr) %) (inc y)]) 1) (range 100000))
                   first
                   (+ (inc xr)))
          nxr (if (= (at-position [(dec nxr) (inc y)]) 0) (dec nxr) nxr)]
      (if (= y 2000) 
        res
        (recur nxl nxr (inc y) (conj res [xl xr]))))))

(defn print-beam [m]
  (for [x (range 100) y (range 100)]
    (do
      (if (= (get m [y x]) 0)
        (print ".")
        (print "#"))
      (when (= y 99) (println)))))

(defn day19-b [input]
  (let [input (->> input
                   read-input2
                   vec)
        at-position (memoize (fn [pos] (run-program6 input pos)))
        v (vec (cons [0 1] (cons [] (create-vec at-position))))]
    (loop [i 0]
      (assert (< i (count v)))
      (let [[x1 x2] (nth v i)
            [x3 y4] (nth v (+ i 99))]
        
        (if (and (not (nil? x1)) (>= (- x2 x3) 100))
          [x3 i]
          (recur (inc i)))))
    ;; (->>
    ;;  (for [x (range 100) y (range 100)]
    ;;    [x y])
    ;;  (reduce #(assoc %1 %2 (at-position %2)) {})
    ;;  (print-beam)
    ;;  )
    ))

;; (day19-b "resources/input19.txt")
