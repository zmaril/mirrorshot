(ns mirrorshot.records
  (:import   (java.awt Graphics Graphics2D Color Polygon)
             (java.awt.image BufferedImage PixelGrabber))
  (:use mirrorshot.util))

;;Records
(defrecord Simple-Color [red blue green alpha])
(defrecord Simple-Point [x y])
(defrecord Simple-Polygon [color points])
(defrecord DNA-Seq [polygons])
(defrecord Critter [DNA buffer fitness])


;;Methods for drawing polygons
(defn draw-polygon [g polygon]
  (let [new-color  (Color. (:red   (:color polygon))
                           (:blue  (:color polygon))
                           (:green (:color polygon) )
                           (:alpha (:color polygon)))
        poly  (let [jpolygon (new Polygon)]
                (doseq [p (:points polygon)]
                  (.addPoint jpolygon (:x p) (:y p)))
                jpolygon)]
    (doto g 
      (.setColor new-color)
      (.fillPolygon poly))))

(defn draw-dna-seq [dna {:keys [width height]}]
  (let [polygons (:polygons dna)
        generated-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphic (.getGraphics generated-image)]
    (doseq [polygon polygons]
      (draw-polygon graphic polygon))
    generated-image))