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

(defn day2-b [input]
  (loop [noun 0
         verb 0]
    (if (= (first (day2-a-beta input noun verb)) 19690720)
      (+ (* 100 noun) verb)
      (recur (if (= verb 99) (inc noun) noun)
             (if (= verb 99) 0 (inc verb))))))

(day2-b (read-input2 "resources/input2.txt"))
