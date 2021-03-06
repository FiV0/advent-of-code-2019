(ns advent-of-code-2019.day18
  (:require [advent-of-code-2019.core :refer [parse-int]]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn input-to-map [input]
  (loop [ls (seq input) x 0 y 0 res {}]
    (cond (empty? ls) res
          (= \newline (first ls)) (recur (rest ls) 0 (inc y) res)
          :else (recur (rest ls) (inc x) y (assoc res [x y] (first ls))))))

(defn read-input [file]
  (-> (slurp file)
      input-to-map))

(def nhood [[0 1] [1 0] [-1 0] [0 -1]])

(defn add-vectors [[x y] [z t]]
  [(+ x z) (+ y t)])

(defn lowercase-to-index [char]
  (- (int char) (int \a)))

(defn uppercase-to-index [char]
  (- (int char) (int \A)))

(defn calculate-path [start end m]
  (loop [seen #{}
         queue (conj clojure.lang.PersistentQueue/EMPTY [start 0 '()])]
    (let [[pos dis doorset] (peek queue)]
      (assert (not (empty? queue)))
      (if (= pos end)
        [dis doorset]
        (let [[newqueue newseen]
              (reduce (fn [[q seen] dir]
                        (let [npos (add-vectors pos dir)
                              c (get m npos)]
                          (cond (or (nil? c) (contains? seen npos) (= c \#)) [q seen] 
                                (Character/isUpperCase c)
                                [(conj q [npos (inc dis) (cons c doorset)]) (conj seen npos)]
                                :else [(conj q [npos (inc dis) doorset])
                                       (conj seen npos)])))
                      [(pop queue) seen] nhood)]
          (recur newseen newqueue))))))

(defn doorset-to-bitset [doorset]
  (reduce (fn [bs c] (bit-set bs (uppercase-to-index c))) 0 doorset))

(defn filter-by-val [pred m]
  (filter (fn [[k v]] (pred v)) m))

(def ^:dynamic *nb-keys* 27)
(def ^:dynamic *graph* nil)

(defn calculate-paths [m]
  (let [start (first (filter-by-val #(= % \@) m))
        keys (sort #(< (int (second %1)) (int (second %2)))
                   (filter-by-val #(Character/isLowerCase %) m))
        nb-keys (count keys)
        all-positions (conj (vec (map first keys)) (first start))]
    (->> (for [i (range (inc nb-keys))
               j (range (inc nb-keys))
               :when (< i j)]
           [i j])
         (reduce (fn [graph [i j]]
                   (let [pos1 (nth all-positions i)
                         pos2 (nth all-positions j)
                         [dis doorset] (calculate-path pos1 pos2 m)
                         bitdoorset (doorset-to-bitset doorset)]
                     (-> (assoc graph [i j] [dis bitdoorset])
                         (assoc [j i] [dis bitdoorset])))) {})
         (vector (inc nb-keys)))))

(defn bit-get [x n]
  (if (bit-test x n) 1 0))

(defn bit-subset [i j]
  (reduce #(and %1 (<= (bit-get i %2) (bit-get j %2))) true (range *nb-keys*)))

(def collect-all-keys
  (memoize (fn [i collected]
             (if (= collected (dec (bit-shift-left 1 *nb-keys*)))
               0
               (loop [j 0 res Integer/MAX_VALUE]
                 (cond (= j (dec *nb-keys*)) res
                       (or (= i j) (= (bit-get collected j) 1))
                       (recur (inc j) res)
                       :else (let [[dis doorset] (get *graph* [i j])]
                               (if (bit-subset doorset collected)
                                 (recur (inc j)
                                        (min res
                                             (+ dis (collect-all-keys j (bit-set collected j)))))
                                 (recur (inc j) res)))))))))

(defn day18-a [file]
  (let [[nb-keys graph]
        (->> (read-input file)
             calculate-paths)]
    (binding [*nb-keys* nb-keys
              *graph* graph]
      (collect-all-keys (dec nb-keys) (bit-set 0 (dec nb-keys))))))

;; (day18-a "resources/input18.txt")
;; (day18-a "resources/test18a.txt")
;; (day18-a "resources/test18b.txt")
;; (day18-a "resources/test18c.txt")
;; (day18-a "resources/test18d.txt")
;; (day18-a "resources/test18e.txt")

(defn calculate-path2 [start end m]
  (loop [seen #{}
         queue (conj clojure.lang.PersistentQueue/EMPTY [start 0 '()])]
    (let [[pos dis doorset] (peek queue)]
      (cond (empty? queue) nil
            (= pos end) [dis doorset]
            :else (let [[newqueue newseen]
                        (reduce (fn [[q seen] dir]
                                  (let [npos (add-vectors pos dir)
                                        c (get m npos)]
                                    (cond (or (nil? c) (contains? seen npos) (= c \#)) [q seen] 
                                          (Character/isUpperCase c)
                                          [(conj q [npos (inc dis) (cons c doorset)]) (conj seen npos)]
                                          :else [(conj q [npos (inc dis) doorset])
                                                 (conj seen npos)])))
                                [(pop queue) seen] nhood)]
                    (recur newseen newqueue))))))

(defn calculate-graph [m positions]
  (let [n (count positions)]
    (->> (for [i (range n) j (range n) :when (< i j)]
           [i j])
         (reduce (fn [graph [i j]]
                   (let [pos1 (nth positions i)
                         pos2 (nth positions j)
                         [dis doorset] (calculate-path2 pos1 pos2 m)]
                     (if (nil? dis)
                       graph
                       (let [bitdoorset (doorset-to-bitset doorset)]
                         (-> (assoc graph [i j] [dis bitdoorset])
                             (assoc [j i] [dis bitdoorset])))))) {}))))

(defn calculate-paths2 [m]
  (let [starts (filter-by-val #(= % \@) m)
        keys (sort #(< (int (second %1)) (int (second %2)))
                   (filter-by-val #(Character/isLowerCase %) m))
        nb-keys (count keys)
        key-positions (vec (map first keys))]
    (->> (vec starts)
         (mapv #(conj key-positions (first %)))
         (mapv #(calculate-graph m %))
         (vector (inc nb-keys)))))

(def ^:dynamic *graphs* nil)

(def collect-all-keys2
  (memoize (fn [robots collected]
             (if (= collected (dec (bit-shift-left 1 *nb-keys*)))
               0
               (loop [j 0 i 0 res Integer/MAX_VALUE]
                 (cond (= i 4) res
                       (= j (dec *nb-keys*)) (recur 0 (inc i) res)
                       (or (= (nth robots i) j) (= (bit-get collected j) 1))
                       (recur (inc j) i res)
                       :else (let [[dis doorset] (get (get *graphs* i) [(get robots i) j])]
                               (if (and (not (nil? dis)) (bit-subset doorset collected))
                                 (recur (inc j) i
                                        (min res (+ dis (collect-all-keys2
                                                         (assoc robots i j)
                                                         (bit-set collected j)))))
                                 (recur (inc j) i res)))))))))

(defn day18-b [file]
  (let [[nb-keys graphs] (->> (read-input file)
                              calculate-paths2)]
    (binding [*nb-keys* nb-keys
              *graphs* graphs]
      (collect-all-keys2 (-> (repeat 4 (dec nb-keys)) vec) (bit-set 0 (dec nb-keys))))))

;; (day18-b "resources/test18-2b.txt")
;; (day18-b "resources/test18-2c.txt")
;; (day18-b "resources/test18-2d.txt")
;; (day18-b "resources/test18-2e.txt")
;; (time (day18-b "resources/input18b.txt"))
