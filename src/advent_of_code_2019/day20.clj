(ns advent-of-code-2019.day19
  (:require [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn input-to-map [input]
  (loop [ls (seq input) x 0 y 0 res {}]
    (cond (empty? ls) res
          (= \newline (first ls)) (recur (rest ls) 0 (inc y) res)
          (= \space (first ls)) (recur (rest ls) (inc x) y res) 
          :else (recur (rest ls) (inc x) y (assoc res [x y] (first ls))))))

(defn read-input [file]
  (-> (slurp file)
      input-to-map))

(def nhood [[0 1] [1 0] [-1 0] [0 -1]])

(defn apply-two-step-nhood [[x y]]
  (map (fn [[i j]] [[x y] [(+ x i) (+ y j)] [(+ x i i) (+ y j j)]]) nhood))

(defn add-vectors [[ax ay] [bx by]] [(+ ax bx) (+ ay by)])

(defn subtract-vectors [[ax ay] [bx by]] [(- ax bx) (- ay by)])

(defn build-portal-map [m]
  (let [max-x (->> (keys m)
                   (map first)
                   (apply max))
        max-y (->> (keys m)
                   (map second)
                   (apply max))
        location-map (->> (for [x (range (inc max-x))
                                y (range (inc max-y))]
                            [x y])
                          (map apply-two-step-nhood)
                          (apply concat)
                          (filter (fn [[pos1 pos2 pos3]]
                                    (and (every? false? (map #(nil? (get m %)) [pos1 pos2 pos2]))
                                         (Character/isUpperCase (get m pos1))
                                         (Character/isUpperCase (get m pos2))
                                         (= (get m pos3) \.))))
                          (map (fn [[pos1 pos2 pos3]]
                                 (let [[x y] (subtract-vectors pos2 pos3)]
                                   (if (or (neg? x) (neg? y))
                                     [(str (get m pos2) (get m pos1)) pos3]
                                     [(str (get m pos1) (get m pos2)) pos3]))))
                          (reduce (fn [res [s pos]]
                                    (if (nil? (get res s))
                                      (assoc res s [pos])
                                      (update res s #(conj % pos)))) {})
                          )
        start (first (get location-map "AA"))
        end (first (get location-map "ZZ"))
        portal-map (->> (dissoc location-map "AA" "ZZ")
                        vals
                        (reduce (fn [res [pos1 pos2]]
                                  (-> (assoc res pos1 pos2)
                                      (assoc pos2 pos1))) {}))]
    [start end portal-map]))

(defn add-neigbours [pos dis m queue seen]
  (reduce (fn [[nq ns] dir]
            (let [next (add-vectors pos dir)]
              (if (and (= (get m next) \.) (not (contains? ns next)))
                [(conj nq [next (inc dis)]) (conj ns next)]
                [nq ns]))) [queue seen] nhood))

(defn bfs [start end m portal-map]
  (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) [start 0])
         seen #{start}]
    (assert (not (empty? queue)))
    (let [[cur dis] (peek queue)]
      (if (= cur end)
        dis
        (let [[new-queue new-seen] (add-neigbours cur dis m (pop queue) seen)]
          (if (or (nil? (get portal-map cur))
                  (contains? new-seen (get portal-map cur)))
            (recur new-queue new-seen)
            (recur (conj new-queue [(get portal-map cur) (inc dis)])
                   (conj new-seen (get portal-map cur)))))))))

(defn day20-a [file]
  (let [m (read-input file)
        [start end portal-map] (build-portal-map m)]
    (bfs start end m portal-map)))

(day20-a "resources/input20.txt")
(day20-a "resources/test20.txt")
