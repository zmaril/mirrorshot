(ns mirrorshot.core
  (:import (java.awt Graphics Graphics2D Color Polygon)
           (java.awt.image BufferedImage PixelGrabber)
           (java.io File)
           (javax.imageio ImageIO)
           (javax.swing JFrame JPanel)
           (mirrorshot.records Critter))
  (:require [mirrorshot.records :as r]
            [mirrorshot.mutate  :as m])
  (:use mirrorshot.util))

(defn -main []
  (let [image (-> "pic.jpg"
                     (File.)
                     ImageIO/read)
        fittest (atom [m/initial-critter])
        jframe (JFrame. "String")
        settings {:width  (.getWidth image)
                  :height (.getHeight image)
                  :src (grab-pixels image)
                  :select-rate 1
                  :callback (fn [i f]
                                  (swap! fittest (fn [o n] n) f)
                                  (.repaint jframe))}]
    (doto jframe
      (.setSize (:width settings) (:height settings))
      (.add (proxy [JPanel] []
              (paint [g]
                (doto g 
                  (.setColor Color/white)
                  (.fillRect 0 0 (:width settings) (:height settings))
                  (.drawImage (:buffer (first @fittest)) nil 0 0)))))
      (.setVisible true))
    (m/evolve settings)))