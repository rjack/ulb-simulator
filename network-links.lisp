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
;;     a-in-port  --> +--------------+ --> b-out-port
;;                    | NETWORK-LINK |
;;                    +--------------+
;;      iface a       | bandwidth    |       iface b
;;                    | delay        |
;;                    | error rate   |
;;     a-out-port <-- +--------------+ <-- b-in-port
;;
;; handle-input udp-packet: output to other endpoint


(in-package :ulb-sim)


(defclass a-in-port (packet-in-port)
  nil)


(defclass a-out-port (packet-out-port)
  nil)


(defclass b-in-port (packet-in-port)
  nil)


(defclass b-out-port (packet-out-port)
  nil)


(defclass network-link (simulator)
  ((bandwidth
    :initarg :bandwidth
    :initform (error ":bandwidth missing")
    :accessor bandwidth-of
    :type number)
   (delay
    :initarg :delay
    :initform (error ":delay missing")
    :accessor delay-of
    :type time-type)
   (error-rate
    :initarg :error-rate
    :initform (error ":error-rate missing")
    :accessor error-rate-of
    :type (mod 100))
   (to-a
    :initform (list)
    :accessor to-a-of
    :type list)
   (to-b
    :initform (list)
    :accessor to-b-of
    :type list)
   (a-out
    :accessor a-out-of
    :type a-out-port)
   (a-in
    :accessor a-in-of
    :type a-in-port)
   (b-out
    :accessor b-out-of
    :type b-out-port)
   (b-in
    :accessor b-in-of
    :type b-in-port)))




(defmethod out-port-ready ((nl network-link)
			   (b-out b-out-port))
  (when (to-b-of nl)
    (send-to-b nl)))

(defmethod in-port-ready ((nl network-link)
			  (b-out b-out-port))
  (when (to-b-of nl)
    (send-to-b nl)))

(defmethod out-port-ready ((nl network-link)
			   (a-out a-out-port))
  (when (to-a-of nl)
    (send-to-a nl)))

(defmethod in-port-ready ((nl network-link)
			  (a-out a-out-port))
  (when (to-a-of nl)
    (send-to-a nl)))




(defmethod handle-input ((nl network-link)
			 (a-in a-in-port)
			 (pkt packet))
  (call-next-method)
  (with-accessors ((to-b to-b-of)) nl
    (let ((must-start-p (null to-b)))
      (setf to-b (append to-b
			 (list pkt)))
      (when must-start-p
	(send-to-b nl)))))


(defmethod handle-input ((nl network-link)
			 (b-in b-in-port)
			 (pkt packet))
  (call-next-method)
  (with-accessors ((to-a to-a-of)) nl
    (let ((must-start-p (null to-a)))
      (setf to-a (append to-a
			 (list pkt)))
      (when must-start-p
	(send-to-a nl)))))




(defmethod leaving ((nl network-link) (a-out a-out-port) (pkt packet))
  (with-accessors ((to-a to-a-of)) nl
    (assert (obj= pkt (first to-a)) nil
	    "leaving nl a-out pkt: not the first pkt to-a!")
    (pop to-a)))


(defmethod leaving ((nl network-link) (a-out a-out-port) (pkt packet))
  (with-accessors ((to-b to-b-of)) nl
    (assert (obj= pkt (first to-b)) nil
	    "leaving nl a-out pkt: not the first pkt to-b!")
    (pop to-b)))




(defmethod output ((nl network-link) (out out-port) (pkt packet))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))



(defmethod send-to-a ((nl network-link))
  (with-accessors ((to-a to-a-of)
		   (a-out a-out-of)
		   (bandwidth bandwidth-of)
		   (error-rate error-rate-of)
		   (delay delay-of)
		   (clock clock-of)) nl
    (assert to-a nil "send-to-a has nothing to send")
    (let ((fst (first to-a)))
      (if (< (random 101) error-rate)
	  ;; transmission error! packet discarded
	  (remove-child nl fst)
	  ;; no transmission error, packet delivered
	  (list (make-instance 'event
			       :time (+ clock
					delay
					(/ (size fst)
					   bandwidth))
			       :owner nl
			       :fn #'output
			       :args (list a-out
					   fst)))))))


(defmethod send-to-b ((nl network-link))
  (with-accessors ((to-b to-b-of)
		   (b-out b-out-of)
		   (bandwidth bandwidth-of)
		   (error-rate error-rate-of)
		   (delay delay-of)
		   (clock clock-of)) nl
    (assert to-b nil "send-to-b has nothing to send")
    (let ((fst (first to-b)))
      (if (< (random 101) error-rate)
	  ;; transmission error! packet discarded
	  (remove-child nl fst)
	  ;; no transmission error, packet delivered
	  (list (make-instance 'event
			       :time (+ clock
					delay
					(/ (size fst)
					   bandwidth))
			       :owner nl
			       :fn #'output
			       :args (list b-out
					   fst)))))))
