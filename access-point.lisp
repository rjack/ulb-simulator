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
;;     wlan-data-in-port  --> +--------------+ --> eth-data-out-port
;;           WLAN0            | ACCESS-POINT |           ETH0
;;     wlan-data-out-port <-- +--------------+ <-- eth-data-in-port
;;
;; handle-input wlan0: output payload to ETH0
;;                     output wifi frame ack to WLAN
;; handle-input eth0: output wifi-frame to WLAN



(in-package :ulb-sim)


(defclass wlan-data-in-port (data-in-port)
  nil)

(defclass wlan-data-out-port (data-out-port)
  nil)

(defclass eth-data-in-port (data-in-port)
  nil)

(defclass eth-data-out-port (data-out-port)
  nil)


(defclass access-point (simulator)
  ((to-wlan-data
    :initform (list)
    :accessor to-wlan-data-of
    :type list)
   (to-eth-data
    :initform (list)
    :accessor to-eth-data-of
    :type list)
   (wlan-data-in
    :accessor wlan-data-in-of
    :type wlan-data-in-port)
   (wlan-data-out
    :accessor wlan-data-out-of
    :type wlan-data-out-port)
   (eth-data-in
    :accessor eth-data-in-of
    :type eth-data-in-port)
   (eth-data-out
    :accessor eth-data-out-of
    :type eth-data-out-port)))



(defmethod in-port-ready ((ap access-point)
			  (wlan-data-out wlan-data-out-port))
  (when (to-wlan-data-of ap)
    (send-to-wlan ap)))

(defmethod out-port-ready ((ap access-point)
			   (wlan-data-out wlan-data-out-port))
  (when (to-wlan-data-of ap)
    (send-to-wlan ap)))

(defmethod in-port-ready ((ap access-point)
			  (eth-data-out eth-data-out-port))
  (when (to-eth-data-of ap)
    (send-to-eth ap)))

(defmethod out-port-ready ((ap access-point)
			   (eth-data-out eth-data-out-port))
  (when (to-eth-data-of ap)
    (send-to-eth ap)))



(defmethod enqueue-to-eth ((ap access-point) (da data))
  (setf (to-eth-data-of ap)
	(append (to-eth-data-of ap)
		(list da))))


(defmethod enqueue-to-wlan ((ap access-point) (da data))
  (setf (to-wlan-data-of ap)
	(append (to-wlan-data-of ap)
		(list da))))


(defmethod handle-input ((ap access-point)
			 (wlan-data-in wlan-data-in-port)
			 (da data))
  "wlan -> eth"
  (call-next-method)
  (with-accessors ((to-eth-data to-eth-data-of)
		   (to-wlan-data to-wlan-data-of)) ap
    (let ((must-send-eth-p (null to-eth-data))
	  (must-send-wlan-p (null to-wlan-data)))
      (enqueue-to-wlan ap (make-instance 'data
					 :payload-size (bytes 32)))
      (enqueue-to-eth ap da)
      (nconc (when must-send-eth-p
	       (send-to-eth ap))
	     (when must-send-wlan-p
	       (send-to-wlan ap))))))


(defmethod handle-input ((ap access-point)
			 (eth-data-in eth-data-in-port)
			 (da data))
  "eth -> wlan"
  (call-next-method)
  (with-accessors ((to-wlan-data to-wlan-data-of)) ap
    (let ((must-send-p (null to-wlan-data)))
      (enqueue-to-wlan ap da)
      (when must-send-p
	(send-to-wlan ap)))))



(defmethod output ((ap access-point) (out out-port) (obj object))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))



(defmethod send-to-wlan ((ap access-point))
  (with-accessors ((to-wlan-data to-wlan-data-of)
		   (clock clock-of)
		   (wlan-data-out wlan-data-out-of)) ap
    (assert to-wlan-data nil
	    "send-to-wlan has nothing to send!")
    (list (make-instance 'event
			 :time clock
			 :owner ap
			 :fn #'output
			 :args (list wlan-data-out
				     (first to-wlan-data))))))


(defmethod send-to-eth ((ap access-point))
  (with-accessors ((to-eth-data to-eth-data-of)
		   (clock clock-of)
		   (eth-data-out eth-data-out-of)) ap
    (assert to-eth-data nil
	    "send-to-eth has nothing to send!")
    (list (make-instance 'event
			 :time clock
			 :owner ap
			 :fn #'output
			 :args (list eth-data-out
				     (first to-eth-data))))))


(defmethod remove-child ((ap access-point) (da data))
  (with-accessors ((to-wlan-data to-wlan-data-of)
		   (to-eth-data to-eth-data-of)) ap
    (cond ((obj= da (first to-wlan-data))
	   (pop to-wlan-data))
	  ((obj= da (first to-eth-data))
	   (pop to-eth-data))
	  ((null (find da to-wlan-data))
	   (error "Removing the wrong child")))
    (call-next-method)))
