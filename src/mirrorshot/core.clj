(ns mirrorshot.core
  (:import (java.awt.image BufferedImage PixelGrabber)
           (java.io File)
           (java.awt Graphics Graphics2D Color Polygon)
           (javax.imageio ImageIO)
           (javax.swing JFrame JPanel))
  (:require [mirrorshot.records :as r]
            [mirrorshot.mutate  :as m])
  (:use mirrorshot.util))

(def sample-size 10)

(set! *warn-on-reflection* true)

(defn -main []
  (let [image (-> "pic.jpg"
                  (File.)
                  ImageIO/read)
        fittest (atom [m/initial-critter])
        src (grab-pixels image)
        settings {:width  (.getWidth image)
                  :height (.getHeight image)
                  :src src
                  :select-rate 1
                  ;;TODO: generation print rate
                  :callback (fn [i next-fittest]
                              (swap! fittest (fn [o n] n) next-fittest)
                              (ImageIO/write ^BufferedImage (:buffer (first @fittest))
                                             "PNG"
                                             ^File (File. "output/best.png"))
                              (ImageIO/write ^BufferedImage (:buffer (first @fittest))
                                             "PNG"
                                             ^File (File. (str "output/gen-" i ".png"))))
                  :sample-size sample-size}]
    (m/evolve settings)))