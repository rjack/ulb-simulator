(declaim (optimize debug safety (speed 0)))
;(declaim (optimize (debug 0) (safety 0) speed))


(in-package :ulb-sim)



(defclass udp-packet (object)
  ((payload
    :initarg :payload
    :initform (error ":payload missing")
    :accessor payload-of)))


(defclass voice (object)
  ((duration
    :initarg :duration
    :initform (error ":duration missing")
    :reader duration-of
    :type (fixnum 0))))
