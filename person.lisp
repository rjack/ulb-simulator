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
;;     | PERSON |
;;     +--------+ <-- voice-in-port


(in-package :ulb-sim)



(defclass person (simulator)
  ((name
    :initarg :name
    :initform (error ":name missing")
    :reader name-of
    :type string)
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
    (format s "path: ~a name: ~a" (path p) (name-of p))))



;; PERSON BEHAVIOUR


(defmethod do-output ((p person) (evs list) (voice-out voice-out-port)
		      (vo voice))
  (call-next-method)



(defmethod talk ((p person) (evs list) (duration fixnum))
  (let ((vo (make-instance 'voice :duration duration)))
    (do-output (add-child p vo) evs (voice-out-of p) vo)))




(defmethod handle-input ((p person) (evs list) (in voice-in-port)
			 (vo voice))
  "Nothing to do."
  (values p evs))
