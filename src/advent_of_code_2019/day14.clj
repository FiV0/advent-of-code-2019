(ns advent-of-code-2019.day14
  (:require [advent-of-code-2019.core :refer [parse-int]]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn read-input [file]
  (as-> file v
    (slurp v)
    (clojure.string/split v #"\n")
    (map #(clojure.string/split % #"=>") v)
    (map (fn [[input endproduct]]
           [(->> (clojure.string/split input #",")
                 (mapv #(->> (clojure.string/split % #" ")
                             (remove empty? )))
                 (map (fn [[n chemical]] [(parse-int n) chemical]))
                 )
            (as-> (clojure.string/split endproduct #" ") v
              (remove empty? v)
              [(parse-int (first v)) (second v)])]) v)))

(defn get-non-ore-chemical [targets]
  (if (= (first (first targets)) "ORE")
    (second targets)
    (first targets)))

(defn get-chem-multiplier [equation-nb nb]
  (bigint (+ (/ nb equation-nb) (if (= 0 (mod nb equation-nb)) 0 1))))

(defn get-overload [chem over]
  (if (nil? (get over chem)) 0 (get over chem)))

(defn get-number-from-overload [chem nb over]
  (let [nb-over (get-overload chem over)
        change (if (> nb nb-over) nb-over nb)]
    [(max 0 (- nb nb-over)) (assoc over chem (- nb-over change))]))

(defn reduce-chemicals [targets over chem-mapping]
  ;; (println targets over)
  (if (= (keys targets) (list "ORE")) (get targets "ORE")
      (let [[chem nb] (get-non-ore-chemical targets)
            [nb newover] (get-number-from-overload chem nb over)]
        (if (= 0 nb)
          (-> (dissoc targets chem)
              (reduce-chemicals newover chem-mapping))
          (let [[equation-nb chem-list] (get chem-mapping chem)
                mulitplier (get-chem-multiplier equation-nb nb)
                chem-list (map (fn [[nb chem]] [(* nb mulitplier) chem]) chem-list)
                nb-over (- (* mulitplier equation-nb) nb)]
            (as-> (dissoc targets chem) v
              (reduce (fn [targets [nb chem]]
                        (update targets chem (fn [x] (if (nil? x) nb (+ x nb))))) v chem-list)
              (reduce-chemicals v (update newover chem (fn [x] (if (nil? x) nb-over (+ x nb-over)))) chem-mapping)))))))

(defn day14-a [file]
  (->> file
       read-input
       (reduce (fn [m [in-chems out-chem]]
                 (assert (nil? (get m (second out-chem))))
                 (assoc m (second out-chem) [(first out-chem) in-chems])) {})
       (reduce-chemicals {"FUEL" 1} {})))

;; (day14-a "resources/input14.txt")
;; (day14-a "resources/test14a.txt")
;; (day14-a "resources/test14b.txt")
;; (day14-a "resources/test14c.txt")
;; (day14-a "resources/test14d.txt")
;; (day14-a "resources/test14e.txt")

(defn find-max-fuel [ores chem-mapping]
  (loop [l 0 r ores]
    (if (= l r) l
        (let [m (inc (bigint (/ (+' l r) 2)))
              res (reduce-chemicals {"FUEL" m} {} chem-mapping)]
          (if (> res ores)
            (recur l (dec m))
            (recur m r))))))

(defn day14-b [file]
  (->> file
       read-input
       (reduce (fn [m [in-chems out-chem]]
                 (assert (nil? (get m (second out-chem))))
                 (assoc m (second out-chem) [(first out-chem) in-chems])) {})
       (find-max-fuel 1000000000000N)))

;; (day14-b "resources/input14.txt")
;; (day14-b "resources/test14c.txt")
;; (day14-b "resources/test14d.txt")
;; (day14-b "resources/test14e.txt")
