(ns mirrorshot.mutate
  (:import (mirrorshot.records DNA-Seq Simple-Polygon Simple-Color Simple-Point Critter))
  (:require [mirrorshot.records :as r] )
  (:use mirrorshot.util))


(def number-of-polygons 3)
(def number-of-vertices 6)
(def initial-strand (DNA-Seq. (for [i (range number-of-polygons)]
                                   (Simple-Polygon.
                                    (Simple-Color. 0 0 0 0)
                                    (for [j (range number-of-vertices)]
                                      (Simple-Point. 0 0))))))

(def initial-critter (Critter. initial-strand nil -100))

(defn mutate [dna settings]
  (update-in dna
             [:polygons]
             (fn [polygons]
               (replace-item polygons (rand-int number-of-polygons)
                             (Simple-Polygon.
                              (Simple-Color. (rand) (rand) (rand) (rand))
                              (for [j (range number-of-vertices)]
                                (Simple-Point. (rand-int (:width settings))
                                               (rand-int (:height settings)))))))))

(defn fitness [critter settings]
  (-> critter
      (assoc :buffer (r/draw-dna-seq (:dna-strand critter) settings))
      (assoc :fitness (rand))))

(defn select [population settings]
  (take (:select-rate settings)
        (sort-by :fitness
                 (pmap (fn [i] (fitness i settings))
                       population))))

(defn evolve [settings]
  (loop [i 0
         population (list initial-critter)]
    (let [fittest (select population settings)
          newborns (map (fn [i] (mutate i settings)) fittest)]
      (when (not= (first population)
                  (first fittest))
        ((:callback settings) i fittest)
        (recur (inc i) (concat fittest newborns))))))