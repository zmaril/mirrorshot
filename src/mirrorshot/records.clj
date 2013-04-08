(ns mirrorshot.records
  (:import   (java.awt Graphics Graphics2D Color Polygon)
             (java.awt.image BufferedImage PixelGrabber))
  (:use mirrorshot.util))

(set! *warn-on-reflection* true)

;;Records
(defrecord Simple-Color [red blue green alpha])
(defrecord Simple-Point [x y])
(defrecord Simple-Polygon [color points])
(defrecord DNA-Seq [polygons])
(defrecord Critter [DNA buffer fitness])


;;Methods for drawing polygons
(defn draw-polygon [^Graphics2D g polygon]
  (let [new-color  (Color. ^Integer (:red   (:color polygon))
                           ^Integer (:blue  (:color polygon))
                           ^Integer (:green (:color polygon))
                           ^Integer (:alpha (:color polygon)))
        poly  (let [jpolygon (new Polygon)]
                (doseq [p (:points polygon)]
                  (.addPoint jpolygon (:x p) (:y p)))
                jpolygon)]
    (doto g 
      (.setColor ^Color new-color)
      (.fillPolygon ^Polygon poly))))

(defn draw-dna-seq [dna {:keys [width height]}]
  (let [polygons (:polygons dna)
        generated-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphic (.getGraphics generated-image)]
    (doseq [polygon polygons]
      (draw-polygon graphic polygon))
    generated-image))