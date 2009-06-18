(declaim (optimize debug safety (speed 0)))
;(declaim (optimize (debug 0) (safety 0) speed))


(in-package :ulb-sim)



(defclass address ()
  ((name
    :initarg :name
    :initform (error ":name missing")
    :accessor name-of
    :type string)
   (port
    :initarg :port
    :initform (error ":port missing")
    :accessor port-of
    :type string)))


(defclass udp-packet (object)
  ((source
    :initarg :source
    :reader source-of
    :type address)
   (destination
    :initarg :destination
    :reader destination-of
    :type address)
   (payload
    :initarg :payload
    :accessor payload-of)
   (size
    :initarg :size
    :accessor size-of
    :type fixnum)))


(defclass voice (object)
  ((duration
    :initarg :duration
    :initform (error ":duration missing")
    :reader duration-of
    :type fixnum)))
