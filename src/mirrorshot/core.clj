(ns mirrorshot.core
  (:import (java.awt.image BufferedImage PixelGrabber)
           (java.io File)
           (java.awt Graphics Graphics2D Color Polygon)
           (javax.imageio ImageIO)
           (javax.swing JFrame JPanel))
  (:require [mirrorshot.records :as r]
            [mirrorshot.mutate  :as m])
  (:use mirrorshot.util))

(def sample-size 10000)

(defn -main []
  (let [image (-> "pic.jpg"
                  (File.)
                  ImageIO/read)
        fittest (atom [m/initial-critter])
;;        jframe (JFrame. "String")
        src (grab-pixels image)
        settings {:width  (.getWidth image)
                  :height (.getHeight image)
                  :src src
                  :select-rate 1
                  :callback (fn [i next-fittest]
                              (swap! fittest (fn [o n] n) next-fittest)
  ;;                            (.repaint jframe)
                              (ImageIO/write (:buffer (first @fittest))
                                             "PNG"
                                             (File. "output/best.png"))
                              (ImageIO/write (:buffer (first @fittest))
                                             "PNG"
                                             (File. (str "output/gen-" i ".png"))))
                  :sample-size sample-size}]
    ;; (doto jframe
    ;;   (.setSize (:width settings) (:height settings))
    ;;   (.add (proxy [JPanel] []
    ;;           (paint [g]
    ;;             (doto g 
    ;;               (.setColor Color/white)
    ;;               (.fillRect 0 0 (:width settings) (:height settings))
    ;;               (.drawImage (:buffer (first @fittest)) nil 0 0)))))
    ;;   (.setVisible true))
    (m/evolve settings)))