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
;;     udp-in-port  --> +--------------+ --> udp-out-port
;;                      | NETWORK-LINK |
;;                      +--------------+
;;        iface a       | bandwidth    |       iface b
;;                      | delay        |
;;                      | error rate   |
;;     udp-out-port <-- +--------------+ <-- udp-in-port
;;
;; handle-input udp-packet: output to other endpoint


(in-package :ulb-sim)


(defclass a-udp-in-port (udp-in-port)
  nil)


(defclass a-udp-out-port (udp-out-port)
  nil)


(defclass b-udp-in-port (udp-in-port)
  nil)


(defclass b-udp-out-port (udp-out-port)
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
   (to-a-packets
    :initform (list)
    :accessor to-a-packets-of
    :type list)
   (to-b-packets
    :initform (list)
    :accessor to-b-packets-of
    :type list)
   (a-udp-out
    :accessor a-udp-out-of
    :type a-udp-out-port)
   (a-udp-in
    :accessor a-udp-in-of
    :type a-udp-in-port)
   (b-udp-out
    :accessor b-udp-out-of
    :type b-udp-out-port)
   (b-udp-in
    :accessor b-udp-in-of
    :type b-udp-in-port)))


(defmethod handle-input ((nl network-link)
			 (a-udp-in a-udp-in-port)
			 (up udp-packet))
  (call-next-method)
  (with-accessors ((to-b-packets to-b-packets-of)) nl
    (let ((must-start-p (null to-b-packets)))
      (setf to-b-packets (append to-b-packets
				 (list up)))
      (when must-start-p
	(send-to-b nl)))))


(defmethod handle-input ((nl network-link)
			 (b-udp-in b-udp-in-port)
			 (up udp-packet))
  (call-next-method)
  (with-accessors ((to-a-packets to-a-packets-of)) nl
    (let ((must-start-p (null to-a-packets)))
      (setf to-a-packets (append to-a-packets
				 (list up)))
      (when must-start-p
	(send-to-a nl)))))


(defmethod port-ready ((nl network-link)
		       (b-udp-out b-udp-out-port))
  (when (to-b-packets-of nl)
    (send-to-b nl)))


(defmethod port-ready ((nl network-link)
		       (a-udp-out a-udp-out-port))
  (when (to-a-packets-of nl)
    (send-to-a nl)))


(defmethod send-to-a ((nl network-link))
  (with-accessors ((to-a-packets to-a-packets-of)
		   (a-udp-out a-udp-out-of)
		   (bandwidth bandwidth-of)
		   (error-rate error-rate-of)
		   (delay delay-of)
		   (clock clock-of)) nl
    (assert to-a-packets nil "send-to-a has nothing to send")
    (let ((fst (first to-a-packets)))
      (if (< (random 101) error-rate)
	  ;; transmission error! packet discarded
	  (remove-child nl fst)
	  ;; no transmission error, packet delivered
	  (list (make-instance 'event
			       :time (+ clock
					delay
					(/ (size-of fst)
					   bandwidth))
			       :owner nl
			       :fn #'output
			       :args (list a-udp-out
					   fst)))))))


(defmethod send-to-b ((nl network-link))
  (with-accessors ((to-b-packets to-b-packets-of)
		   (b-udp-out b-udp-out-of)
		   (bandwidth bandwidth-of)
		   (error-rate error-rate-of)
		   (delay delay-of)
		   (clock clock-of)) nl
    (assert to-b-packets nil "send-to-b has nothing to send")
    (let ((fst (first to-b-packets)))
      (if (< (random 101) error-rate)
	  ;; transmission error! packet discarded
	  (remove-child nl fst)
	  ;; no transmission error, packet delivered
	  (list (make-instance 'event
			       :time (+ clock
					delay
					(/ (size-of fst)
					   bandwidth))
			       :owner nl
			       :fn #'output
			       :args (list b-udp-out
					   fst)))))))


(defmethod remove-child ((nl network-link) (up udp-packet))
  (with-accessors ((to-b-packets to-b-packets-of)
		   (to-a-packets to-a-packets-of)) nl
    (cond ((eq up (first to-b-packets))
	   (pop to-b-packets))
	  ((eq up (first to-a-packets))
	   (pop to-a-packets))
	  (t (error "removing the wrong child!")))))
