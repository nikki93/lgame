(in-package #:lgame)

(defgeneric deinitialize-instance (thing)
  (:documentation "Deinitialize a thing -- opposite of initialize-instance."))

(defgeneric update (thing dt)
  (:documentation "Step a thing forward by dt."))

(defgeneric draw (thing)
  (:documentation "Draw a thing to the screen."))


