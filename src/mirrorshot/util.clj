(ns mirrorshot.util
  (:import (java.awt.image BufferedImage PixelGrabber))
  (:require [clojure.reflect :as r])
  (:use [clojure.pprint :only (pprint)]))

;;A few methods that are useful for debugging at the repl. Hopefully,
;;this will only be used for development purposes.
(defn members-of-object [object]
  (-> object r/reflect :members))

(defmacro find-member [object prop]
  `(filter #(= '~prop (:name %)) (members-of-object ~object)))

(defn names-of-members [object]
  (sort (distinct (map :name (members-of-object object)))))


(defn remove-item
  "Returns a sequence without the n-th item of s."
  [s n]
  (cond
    (vector? s) (into (subvec s 0 n)
                (subvec s (min (+ n 1) (count s)) (count s)))
    (list? s) (concat (take n s)
                      (drop (inc n) s))))

(defn replace-item
  "Returns a list with the n-th item of l replaced by v."
  [l n v]
  (concat (take n l) (list v) (drop (inc n) l)))

;;Initial parameters and strands
(defn grab-pixels [image]
  (let [w (.getWidth  image)
        h (.getHeight image)
        pixels (make-array Integer/TYPE (* w h))]
    (.grabPixels (PixelGrabber. image 0 0 w h pixels 0 w))
    pixels))

(defn printlnn [a]
  (println a)
  a)