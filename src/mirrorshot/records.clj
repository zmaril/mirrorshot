(ns mirrorshot.records
  (:import   (java.awt Graphics Graphics2D Color Polygon)
             (java.awt.image BufferedImage PixelGrabber)))

;;Records
(defrecord Simple-Color [red blue green alpha])
(defrecord Simple-Point [x y])
(defrecord Simple-Polygon [color points])
(defrecord DNA-Seq [polygons])
(defrecord Critter [DNA buffer fitness])


;;Methods for drawing polygons
(defn draw-polygon [g polygon]
  (doto g
    (.setColor (apply #(Color. %) (juxt [:red :blue :green :alpha] (:color polygon))))
    (.fillPolygon (let [jpolygon (new Polygon)]
                    (doseq [p (:points polygon)]
                      (.addPoint jpolygon (:x p) (:y p)))
                    jpolygon)))
  nil)

(defn draw-dna-seq [{:keys [polygons]} {:keys [width height]}]
  (let [generated-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphic (.getGraphics generated-image)]
    (doseq [polygon polygons]
      (draw-polygon graphic polygon))
    generated-image))