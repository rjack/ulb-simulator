;; ULB-SIM
;; UDP Load Balancing SIMulator

;; Copyright (C) 2009  Giacomo Ritucci

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer in the documentation and/or other materials
;;      provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(declaim (optimize debug safety (speed 0)))
;(declaim (optimize (debug 0) (safety 0) speed))


;; OVERVIEW
;;
;;     +--------+ --> voice-out-port
;;     | PERSON |       SOFTPHONE
;;     +--------+ <-- voice-in-port


(in-package :ulb-sim)



(defclass person (simulator)
  ((name
    :initarg :name
    :initform (error ":name missing")
    :reader name-of
    :type string)
   (to-phone
    :initarg :to-phone
    :initform nil
    :accessor to-phone-of
    :type list
    :documentation "List of voices")
   (voice-in
    :accessor voice-in-of
    :type voice-in-port)
   (voice-out
    :accessor voice-out-of
    :type voice-out-port)))



(defmethod initialize-instance :after ((p person) &key)
  (with-accessors ((voi< voice-in-of) (voi> voice-out-of)) p
    (setf voi< (make-instance 'voice-in-port :owner p))
    (setf voi> (make-instance 'voice-out-port :owner p))))


(defmethod print-object ((p person) s)
  (print-unreadable-object (p s :type t :identity t)
    (format s "~a" (name-of p))))




(defmethod in-port-ready ((p person) (voice-out voice-out-port))
  (when (to-phone-of p)
    (talk p)))

(defmethod out-port-ready ((p person) (voice-out voice-out-port))
  (when (to-phone-of p)
    (talk p)))




(defmethod handle-input ((p person) (in voice-in-port) (vo voice))
  "Nothing to do, just discard received voice."
  (call-next-method)
  (remove-child p vo)
  nil)




(defmethod talk ((p person))
  (with-accessors ((to-phone to-phone-of) (clock clock-of)
		   (voice-out voice-out-of)) p
    (assert (not (null to-phone)) nil
	    "Talk but nothing to say!")
    (list (make-instance 'event
			 :owner p
			 :time clock
			 :fn #'output
			 :args (list voice-out
				     (first to-phone))))))




(defmethod leaving ((p person) (voice-out voice-out-port) (vo voice))
  (with-accessors ((to-phone to-phone-of)) p
    (assert (obj= vo (first to-phone)) nil
	    "leaving p voice-out vo: not the first outgoing-voice!")
    (pop to-phone)))




(defmethod output ((p person) (voice-out voice-out-port) (vo voice))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'abort))
    (call-next-method)))
