(declaim (optimize debug safety (speed 0)))

(defpackage :it.unibo.cs.web.ritucci.ulb-sim
  (:nicknames :ulb)
  (:use :common-lisp :ds))

(in-package :ulb)

(make-instance 'world :id 0)
