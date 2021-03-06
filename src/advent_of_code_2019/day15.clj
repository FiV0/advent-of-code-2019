(ns advent-of-code-2019.day15
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

(def steps [[1 0] [-1 0] [0 1] [0 -1]])

(defn add-vectors [[[x1 y1] s1] [x2 y2]]
  [[(+ x1 x2) (+ y1 y2)] (inc s1)])

(defn calculate-new-front [input i r m1 cur seen]
  (reduce (fn [front j]
            (let [next (add-vectors cur (nth steps (dec j)))]
              (if (contains? seen (first next))
                front
                (conj front [(assoc-expand input (get input (inc i)) j m1 r)
                             (+ i 2) r next]))))
          [] (range 1 5)))

(defn concat-vec [& vs]
  (vec (apply concat vs)))

(defn run-program
  ([input] (run-program input 0 0 [[0 0] 0] #{[0 0]} []))
  ([input i r cur seen front]
   (loop [input input i i r r cur cur seen seen front front]
     (match (int-to-array (get input i))
            [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                              (+' (get-program-value2 input m1 (inc i) r)
                                                  (get-program-value2 input m2 (+ i 2) r))
                                              m3 r)
                                (+ i 4) r cur seen front)
            [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                              (*' (get-program-value2 input m1 (inc i) r)
                                                  (get-program-value2 input m2 (+ i 2) r))
                                              m3 r)
                                (+ i 4) r cur seen front)
            [_ _ m1 3] (let [newfront (calculate-new-front input i r m1 cur seen)]
                         (cond
                           (empty? newfront)
                           (let [[newinput ii rr next] (first front)]
                                (recur newinput ii rr next seen (subvec front 1)))
                           :else
                           (let [[newinput ii rr next] (first newfront)] 
                            (recur newinput ii rr next seen (concat-vec front (subvec newfront 1))))))
            [_ _ m1 4]
            (if (empty? front)
              (case (get-program-value2 input m1 (inc i) r)
                0 (throw (Exception. "Should not happen!!!")) 
                1 (recur input (+ i 2) r cur (conj seen (first cur)) [])
                2 cur)
              (let [[newinput ii rr next] (first front)]
                (case (get-program-value2 input m1 (inc i) r)
                  0 (recur newinput ii rr next (conj seen (first cur)) (subvec front 1)) 
                  1 (recur newinput ii rr next (conj seen (first cur)) (conj (subvec front 1) [input (+ i 2) r cur]))
                  2 cur)))
            [_ m2 m1 5] (if (= (get-program-value2 input m1 (inc i) r) 0)
                          (recur input (+ i 3) r cur seen front)
                          (recur input (get-program-value2 input m2 (+ i 2) r) r cur seen front))
            [_ m2 m1 6] (if (= (get-program-value2 input m1 (inc i) r) 0)
                          (recur input (get-program-value2 input m2 (+ i 2) r) r cur seen front)
                          (recur input (+ i 3) r cur seen front))
            [m3 m2 m1 7] (->
                          (assoc-expand input (get input (+ i 3))
                                        (if (< (get-program-value2 input m1 (inc i) r)
                                               (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                        m3 r)
                          (recur (+ i 4) r cur seen front))
            [m3 m2 m1 8] (->
                          (assoc-expand input (get input (+ i 3))
                                        (if (= (get-program-value2 input m1 (inc i) r)
                                               (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                        m3 r)
                          (recur (+ i 4) r cur seen front))
            [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value2 input m1 (inc i) r)) cur seen front)
            [_ _ _ 99] input
            :else (throw (Exception. "Should not happen!!!"))))))

(defn day15-a [input]
  (->> input
       read-input2
       vec
       run-program))

;; (day15-a "resources/input15.txt")
(def coordinates (first *1))

(defn add-vectors2 [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn calculate-new-front2 [input i r m1 cur seen]
  (reduce (fn [front j]
            (let [next (add-vectors2 cur (nth steps (dec j)))]
              (if (contains? seen next)
                front
                (conj front [(assoc-expand input (get input (inc i)) j m1 r)
                             (+ i 2) r next]))))
          [] (range 1 5)))

(defn run-program2
  ([input] (run-program2 input 0 0 [0 0] {[0 0] 1} []))
  ([input i r cur seen front]
   (loop [input input i i r r cur cur seen seen front front]
     ;; (println i " " (get input i))
     (match (int-to-array (get input i))
            [m3 m2 m1 1] (recur (assoc-expand input (get input (+ i 3))
                                              (+' (get-program-value2 input m1 (inc i) r)
                                                  (get-program-value2 input m2 (+ i 2) r))
                                              m3 r)
                                (+ i 4) r cur seen front)
            [m3 m2 m1 2] (recur (assoc-expand input (get input (+ i 3))
                                              (*' (get-program-value2 input m1 (inc i) r)
                                                  (get-program-value2 input m2 (+ i 2) r))
                                              m3 r)
                                (+ i 4) r cur seen front)
            [_ _ m1 3] (let [newfront (calculate-new-front2 input i r m1 cur seen)]
                         (cond
                           (and (empty? newfront) (empty? front)) seen
                           (empty? newfront)
                           (let [[newinput ii rr next] (first front)]
                             (recur newinput ii rr next seen (subvec front 1)))
                           :else
                           (let [[newinput ii rr next] (first newfront)] 
                             (recur newinput ii rr next seen (concat-vec front (subvec newfront 1))))))
            [_ _ m1 4]
            (if (empty? front)
              (case (get-program-value2 input m1 (inc i) r)
                0 seen 
                1 (recur input (+ i 2) r cur (assoc seen cur 1) [])
                2 (recur input (+ i 2) r cur (assoc seen cur 1) []))
              (let [[newinput ii rr next] (first front)]
                (case (get-program-value2 input m1 (inc i) r)
                  0 (recur newinput ii rr next (assoc seen cur 0) (subvec front 1)) 
                  1 (recur newinput ii rr next (assoc seen cur 1) (conj (subvec front 1) [input (+ i 2) r cur]))
                  2 (recur newinput ii rr next (assoc seen cur 1) (conj (subvec front 1) [input (+ i 2) r cur])))))
            [_ m2 m1 5] (if (= (get-program-value2 input m1 (inc i) r) 0)
                          (recur input (+ i 3) r cur seen front)
                          (recur input (get-program-value2 input m2 (+ i 2) r) r cur seen front))
            [_ m2 m1 6] (if (= (get-program-value2 input m1 (inc i) r) 0)
                          (recur input (get-program-value2 input m2 (+ i 2) r) r cur seen front)
                          (recur input (+ i 3) r cur seen front))
            [m3 m2 m1 7] (->
                          (assoc-expand input (get input (+ i 3))
                                        (if (< (get-program-value2 input m1 (inc i) r)
                                               (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                        m3 r)
                          (recur (+ i 4) r cur seen front))
            [m3 m2 m1 8] (->
                          (assoc-expand input (get input (+ i 3))
                                        (if (= (get-program-value2 input m1 (inc i) r)
                                               (get-program-value2 input m2 (+ i 2) r)) 1 0)
                                        m3 r)
                          (recur (+ i 4) r cur seen front))
            [_ _ m1 9]  (recur input (+ i 2) (+ r (get-program-value2 input m1 (inc i) r)) cur seen front)
            [_ _ _ 99] input
            :else (throw (Exception. "Should not happen!!!"))))))

(defn bfs [[startx starty] seen]
  (loop [res {[startx starty] 0} nexts [[startx starty]]]
    (if (empty? nexts) res
        (let [[x y] (first nexts)
              dis (get res [x y])
              newnexts (reduce (fn [v [i j]]
                                 (cond
                                   (contains? res [(+ x i) (+ y j)]) v
                                   (= (get seen [(+ x i) (+ y j)]) 1) (conj v [(+ x i) (+ y j)])
                                   :else v)) [] steps)
              newres (reduce (fn [res cur]
                               (assoc res cur (inc dis))) res newnexts)]
          (recur newres (concat-vec (subvec nexts 1) newnexts))))))

(defn day15-b [input]
  (->> input
       read-input2
       vec
       run-program2
       (bfs coordinates)
       vals
       (apply max)))

;; (day15-b "resources/input15.txt")
