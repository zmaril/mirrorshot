(ns mirrorshot.mutate
  (:import (mirrorshot.records DNA-Seq Simple-Polygon Simple-Color Simple-Point Critter)
           (java.awt Graphics Graphics2D Color Polygon))

  (:require [mirrorshot.records :as r] )
  (:use mirrorshot.util))


(def number-of-polygons 50)
(def number-of-vertices 6)

(def initial-strand (DNA-Seq. (for [i (range number-of-polygons)]
                                   (Simple-Polygon.
                                    (Simple-Color. (rand-int 255) (rand-int 255)
                                                   (rand-int 255) (rand-int 255))
                                    (vec (for [j (range number-of-vertices)]
                                           (Simple-Point. (rand-int 1000) (rand-int 1000))))))))

(def initial-critter (Critter. initial-strand nil -100))

(def color-delta 255)
(def point-delta 1000)

(defn bound [i j n]
  (max i (min n j)))

(defn mutate-polygon [polygon {:keys [width height]}]
  (-> polygon
      (update-in [:color (rand-nth [:blue :red :green :alpha])]
                 (fn [n] (int (bound 0 255 (+ n (-> color-delta - (/ 2))
                                              (rand-int color-delta))))))
      (update-in [:points (rand-int number-of-vertices)]
                 (fn [point]
                   (-> point
                       (update-in [:x] (fn [x] (bound 0 width
                                                      (int (+ x (-> point-delta - (/ 2))
                                                              (rand-int point-delta))))))
                       (update-in [:y] (fn [y] (bound 0 height
                                                      (int (+ y (-> point-delta - (/ 2))
                                                              (rand-int point-delta)))))))))))
(defn mutate [dna settings]
  (let [results
        (update-in dna [:DNA :polygons]
                   (fn [polygons]
                     (let [n (rand-int number-of-polygons)
                           polygon (nth polygons n)
                           mutated-polygon (mutate-polygon polygon settings)]
                       (replace-item polygons n mutated-polygon))))]
    (if (< 0.25 (rand))
      (mutate results settings)
      results)))

(defn color-distance [a b]
  (let [dr (- (.getRed a) (.getRed b))
        db (- (.getBlue a) (.getBlue b))
        dg (- (.getGreen a) (.getGreen b))]
    (+ (* dr dr) (* dg dg) (* db db ))))

(defn fitness [critter {:keys [src height width] :as settings}]
  (let [buffer (r/draw-dna-seq (:DNA critter) settings)
        fitness  (let [gen (grab-pixels buffer)
                       samples (for [i (range 1000)] (* (rand-int height) (rand-int width)))
                       point-compare (fn [i]
                                       (color-distance (Color. (aget src i))
                                                       (Color. (aget gen i))))]
                   (reduce + (map point-compare samples)))]
    (merge critter {:buffer buffer
                    :fitness fitness})))

(def start-time (System/currentTimeMillis))
(defn select [population settings]
  (let [fitted (pmap (fn [critter] (fitness critter settings)) population)
        sorted (sort-by :fitness fitted)
        mean-fitness (float (/ (reduce + (map :fitness sorted))
                                    (count sorted)))
        seconds-so-far (float (/ (- (System/currentTimeMillis) start-time) 1000))]
    (println seconds-so-far mean-fitness)
    (take (:select-rate settings) sorted)))


(defn evolve [settings]
  (loop [i 0
         population (list initial-critter)]    
    (let [fittest (select population settings)
          newborns (flatten (map #(for [i (range 10)] (mutate % settings)) fittest))]
      (print "Generation" i " ")
      (when-not (= (first population)
                   (first fittest))
        (when (= 0 (mod i 20))          
          ((:callback settings) i fittest))
        (recur (inc i) (concat fittest newborns))))))