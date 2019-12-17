(ns advent-of-code-2019.day17
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
  (loop [input input i 0 r 0 pos [0 0] res {}]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                          (+' (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r pos res)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                          (*' (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r))
                                          m3 r)
                               (+ i 4) r pos res)
           [_ _ m1 3] (do (println "Enter a number:")
                          (let [in (parse-int (read-line))]
                            (recur (assoc-expand input (get input (inc i)) in
                                                 m1 r)
                                   (+ i 2) r pos res)))
           [_ _ m1 4] (let [c (char (get-program-value input m1 (inc i) r))]
                        (case c
                          \newline (if (= (first pos) 0)
                                     res
                                     (recur input (+ i 2) r [0 (inc (second pos))] res))
                          (recur input (+ i 2) r [(inc (first pos)) (second pos)]
                                 (assoc res pos c))))
           [_ m2 m1 5] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (+ i 3) r pos res)
                         (recur input (get-program-value input m2 (+ i 2) r) r pos res))
           [_ m2 m1 6] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (get-program-value input m2 (+ i 2) r) r pos res)
                         (recur input (+ i 3) r pos res))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r pos res))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                       (if (= (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                               (+ i 4) r pos res)
           [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value input m1 (inc i) r)) pos res)
           [_ _ _ 99] input
           :else (throw (Exception. "Should not happen!!!")))))

(def steps [[0 1] [1 0] [-1 0] [0 -1]])

(defn add-vectors [[x y] [i j]]
  [(+ x i) (+ y j)])

(defn neighbours [m vec]
  (->> (map #(add-vectors vec %) steps)
       (map #(get m %))))

(defn scaffold? [c] (and (not= \. c) (not (nil? c))))

(defn neighbours-scaffold [m vec]
  (every? scaffold? (neighbours m vec)))

(defn calc-crossovers [m]
  (->>
   (map (fn [[x y]]
          (if (and (neighbours-scaffold m [x y])
                   (scaffold? (get m [x y])))
             (* x y)
             0)) (keys m))
   (reduce +)))

(defn day17-a [input]
  (->> input
       read-input2
       vec
       run-program
       calc-crossovers))

;; (day17-a "resources/input17.txt")

(defn run-program2
  [input instructions] 
  (loop [input input i 0 r 0 insts instructions]
    (match (int-to-array (get input i))
           [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                             (+' (get-program-value input m1 (inc i) r)
                                                 (get-program-value input m2 (+ i 2) r))
                                             m3 r)
                               (+ i 4) r insts)
           [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                             (*' (get-program-value input m1 (inc i) r)
                                                 (get-program-value input m2 (+ i 2) r))
                                             m3 r)
                               (+ i 4) r insts)
           [_ _ m1 3] (let [in (first insts)]
                        (recur (assoc-expand input (get input (inc i)) in
                                             m1 r)
                               (+ i 2) r (rest insts)))
           [_ _ m1 4] (do (println "Output: " (char (get-program-value input m1 (inc i) r)))
                          (recur input (+ i 2) r insts))
           [_ m2 m1 5] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (+ i 3) r insts)
                         (recur input (get-program-value input m2 (+ i 2) r) r insts))
           [_ m2 m1 6] (if (= (get-program-value input m1 (inc i) r) 0)
                         (recur input (get-program-value input m2 (+ i 2) r) r insts)
                         (recur input (+ i 3) r insts))
           [m3 m2 m1 7] (->
                         (assoc-expand input (get input (+ i 3))
                                       (if (< (get-program-value input m1 (inc i) r)
                                              (get-program-value input m2 (+ i 2) r)) 1 0)
                                       m3 r)
                         (recur (+ i 4) r insts))
           [m3 m2 m1 8] (recur (assoc-expand input (get input (+ i 3))
                                             (if (= (get-program-value input m1 (inc i) r)
                                                    (get-program-value input m2 (+ i 2) r)) 1 0)
                                             m3 r)
                               (+ i 4) r insts)
           [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value input m1 (inc i) r)) insts)
           [_ _ _ 99] input
           :else (throw (Exception. "Should not happen!!!")))))

(defn display-area [m]
  (let [max-x (->> (keys m)
                   (map first)
                   (apply max))
        max-y (->> (keys m)
                   (map second)
                   (apply max))]
    (doseq [x (range 0 (inc max-x))
            y (range 0 (inc max-y))]
      (print (get m [x y]))
      (when (= y max-y) (println)))))

(defn display [input]
  (->> input
       read-input2
       vec
       run-program
       display-area))

(display "resources/input17.txt")

(def step-map {0 [-1 0] 1 [0 1] 2 [1 0] 3 [0 -1]})

;; looked at the map before
(defn calc-instructions [m]
  (let [start (first (keep #(when (= (val %) \^) (key %)) m))]
    (loop [orient 0 cur start res '(0)]
      (let [next (add-vectors cur (get step-map orient))
            orient1 (mod (+ orient 1) 4)  
            orient2 (mod (- orient 1) 4)  
            sidestep1 (add-vectors cur (get step-map orient1))
            sidestep2 (add-vectors cur (get step-map orient2))]
        (cond
          (scaffold? (get m next))
          (recur orient next (cons (inc (first res)) (rest res)))
          (scaffold? (get m sidestep1))
          (recur orient1 cur (cons 0 (cons \R res)))
          (scaffold? (get m sidestep2))
          (recur orient2 cur (cons 0 (cons \L res)))
          :else (reverse res))))))

(defn calc-insts-helper
  ([m orient cur] (calc-insts-helper m orient cur '(0) #{}))
  ([m orient cur res seen]
   (loop [acc '()  stack (list (list orient cur res seen))]
     #break
     (if (empty? stack) (reverse acc)
         (let [[orient cur res seen] (first stack)
               stack (rest stack)
               next (add-vectors cur (get step-map orient))
               scaffold-next (scaffold? (get m next))
               seen-next (contains? seen next)
               orient1 (mod (+ orient 1) 4)  
               orient2 (mod (- orient 1) 4)  
               sidestep1 (add-vectors cur (get step-map orient1))
               scaffold-side1 (scaffold? (get m sidestep1))
               seen-side1 (contains? seen sidestep1)
               sidestep2 (add-vectors cur (get step-map orient2))
               scaffold-side2 (scaffold? (get m sidestep2))
               seen-side2 (contains? seen sidestep2)]
           (cond
             (and scaffold-next scaffold-side1 scaffold-side2 seen-side1 seen-side2)
             (recur acc (cons (list orient next (cons (inc (first res)) (rest res)) (conj seen cur))
                              stack))
             (and scaffold-next scaffold-side1 scaffold-side2 (not seen-side1) (not seen-side2))
             (recur acc (concat (list (list orient next (cons (inc (first res)) (rest res)) (conj seen cur))
                                      (list orient1 cur (cons 0 (cons \R res)) (conj seen cur))
                                      (list orient2 cur (cons 0 (cons \L res)) (conj seen cur)))
                                stack))
             (and scaffold-side1 (not seen-side1))
             (recur acc (cons (list orient1 cur (cons 0 (cons \R res)) (conj seen cur))
                              stack))
             (and scaffold-side2 (not seen-side2))
             (recur acc (cons (list orient2 cur (cons 0 (cons \L res)) (conj seen cur))
                              stack))
             scaffold-next
             (recur acc (cons (list orient next (cons (inc (first res)) (rest res)) (conj seen cur))
                              stack))
             :else (recur (cons (reverse res) acc) )))))))

(defn calc-possible-instructions [m]
  (let [start (first (keep #(when (= (val %) \^) (key %)) m))]
    (->> (calc-insts-helper m 0 start)
         (map expand-numbers))))

(def instructions (->> "resources/input17.txt" 
                       read-input2
                       vec
                       run-program
                       calc-instructions
                       expand-numbers))

(def instructions (->> "resources/input17.txt" 
                       read-input2
                       vec
                       run-program
                       calc-possible-instructions))

(defn fibo-iter
  ([n] (fibo-iter 0 1 n))
  ([cur nxt n]
   (cond
     (zero? n) cur
     :else (recur nxt (+ cur nxt) (dec n)))))

(fibo-iter 10)

;; (calc-possible-instructions )



(defn remove-sublist [ls sub]
  (loop [ls ls res '() cur '()]
    (cond (> (count sub) (count ls))
          (remove empty? (reverse (cons (concat (reverse cur) ls) res)) )
          (= (take (count sub) ls) sub)
          (recur (drop (count sub) ls) (cons (reverse cur) res) '())
          :else (recur (rest ls) res (cons (first ls) cur)))))

;; (remove-sublist '(1 2 1 2 3 4 5 1 2 3 1 2 3 5) '(1 2 3))
;; (remove-sublist '(1 2 1 2 3 4 5 1 2 3 1 2 3 7 8 5) '(1 2))

(defn get-pattern [lists]
  (let [n (min 11 (count (first lists)) )]
    (loop [i 1]
      (cond (> i n) nil
            (->> (map #(remove-sublist % (take i (first lists))) lists)
                 (apply concat)
                 empty?) (take i (first lists))
            :else (recur (inc i))))))

(defn find-patterns-helper [ls i]
  (let [a (take i ls)
        sublists (remove-sublist ls a)]
    (if (empty? sublists) [a '() '()]
        (loop [i 1]
          (if (or (> i (count (first sublists))) (> i 11)) nil
              (let [b (take i (first sublists))
                    subsublists (->> (map #(remove-sublist % b) sublists)
                                     (apply concat))]
                (if (empty? subsublists) [a b '()]
                    (let [c (get-pattern subsublists)]
                      (if (nil? c)
                        (recur (inc i))
                        [a b c])))))))))

;; (find-patterns-helper '(1 2 1 2 3 4 5 3 3 3 4 5 1 2 3) 2)
;; (find-patterns-helper '(1 2 1 2 3 4 5 3 3 3 4 5 1 2 3 4) 2)
;; (find-patterns-helper '(1 2 1 3 1 2 1 4) 3)

(defn find-patterns [ls]
  (loop [i 1]
    (if (> i 11) nil
        (let [res (find-patterns-helper ls i)]
          (if (nil? res) (recur (inc i))
              res)))))

;; (find-patterns '(1 2 1 2 3 4 5 3 3 3 4 5 1 2 3))
;; (find-patterns '(1 2 1 2 3 4 5 3 3 3 4 5 1 2 3 4))


(defn apply-patterns [ls [a b c]]
  (loop [l ls res '()]
    (if (empty? l) (reverse res)
        (cond (= (take (count a) l) a) (recur (drop (count a) l) (cons \A res))
              (= (take (count b) l) b) (recur (drop (count b) l) (cons \B res))
              (= (take (count c) l) c) (recur (drop (count c) l) (cons \C res))
              :else (throw (Exception. "Should not happen!!!"))))))

(defn remove-last [ls]
  (reverse (rest (reverse ls))))

(defn number-to-char [n]
  (first (seq (str n))))

(defn expand-number
  ([n] (expand-number n '()))
  ([n res]
   (if (= n 0) res
       (expand-number (quot n 10) (cons (number-to-char (mod n 10)) res)))))

(defn expand-numbers [ls]
  (cond (empty? ls) ls
        (= (type (first ls)) java.lang.Long)
        (concat (expand-number (first ls) (expand-numbers (rest ls))))
        :else (cons (first ls) (expand-numbers (rest ls)))))


(defn day17-b [input]
  (let [input (->> input read-input2 vec)
        input2 (assoc input 0 2)
        instructions (expand-numbers (calc-instructions (run-program input)))
        patterns (find-patterns instructions)
        routines (apply-patterns instructions patterns)
        patterns (map #(remove-last (interleave % (repeat (count %) \,))) patterns)
        routines (remove-last (interleave routines (repeat (count routines) \, )))
        patterns (map #(concat % '(\newline)) patterns)
        routines (concat routines '(\newline))
        all-instructions (concat routines (apply concat patterns) '(\n \newline))
        ascii-inst (map int all-instructions)]
    (map #(println (count %)) patterns)
    ;; (run-program2 input2 ascii-inst)
    ))

(conj #{1 2 3} 7)

(day17-b "resources/input17.txt")
