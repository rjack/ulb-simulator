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
;;     wlan-in-port  --> +--------------+ --> eth-out-port
;;         WLAN0         | ACCESS-POINT |        ETH0
;;     wlan-out-port <-- +--------------+ <-- eth-in-port
;;
;; handle-input wlan0: output payload to ETH0
;;                     output wifi frame ack to WLAN
;; handle-input eth0: output wifi-frame to WLAN



(in-package :ulb-sim)


(defclass wlan-in-port (data-in-port)
  nil)

(defclass wlan-out-port (data-out-port)
  nil)

(defclass eth-in-port (data-in-port)
  nil)

(defclass eth-out-port (data-out-port)
  nil)


(defclass access-point (simulator)
  ((to-wlan
    :initform (list)
    :accessor to-wlan-of
    :type list)
   (to-eth
    :initform (list)
    :accessor to-eth-of
    :type list)
   (wlan-in
    :accessor wlan-in-of
    :type wlan-in-port)
   (wlan-out
    :accessor wlan-out-of
    :type wlan-out-port)
   (eth-in
    :accessor eth-in-of
    :type eth-in-port)
   (eth-out
    :accessor eth-out-of
    :type eth-out-port)))




(defmethod in-port-ready ((ap access-point)
			  (wlan-out wlan-out-port))
  (when (to-wlan-of ap)
    (send-to-wlan ap)))

(defmethod out-port-ready ((ap access-point)
			   (wlan-out wlan-out-port))
  (when (to-wlan-of ap)
    (send-to-wlan ap)))

(defmethod in-port-ready ((ap access-point)
			  (eth-out eth-out-port))
  (when (to-eth-of ap)
    (send-to-eth ap)))

(defmethod out-port-ready ((ap access-point)
			   (eth-out eth-out-port))
  (when (to-eth-of ap)
    (send-to-eth ap)))





(defmethod enqueue-to-eth ((ap access-point) (da data))
  (setf (to-eth-of ap)
	(append (to-eth-of ap)
		(list da))))


(defmethod enqueue-to-wlan ((ap access-point) (da data))
  (setf (to-wlan-of ap)
	(append (to-wlan-of ap)
		(list da))))




(defmethod handle-input ((ap access-point)
			 (wlan-in wlan-in-port)
			 (da data))
  "wlan -> eth"
  (call-next-method)
  (with-accessors ((to-eth to-eth-of)
		   (to-wlan to-wlan-of)) ap
    (let ((must-send-eth-p (null to-eth))
	  (must-send-wlan-p (null to-wlan)))
      (enqueue-to-wlan ap (make-instance 'data
					 :payload-size (bytes 32)))
      (enqueue-to-eth ap da)
      (nconc (when must-send-eth-p
	       (send-to-eth ap))
	     (when must-send-wlan-p
	       (send-to-wlan ap))))))


(defmethod handle-input ((ap access-point)
			 (eth-in eth-in-port)
			 (da data))
  "eth -> wlan"
  (call-next-method)
  (with-accessors ((to-wlan to-wlan-of)) ap
    (let ((must-send-p (null to-wlan)))
      (enqueue-to-wlan ap da)
      (when must-send-p
	(send-to-wlan ap)))))




(defmethod leaving ((ap access-point) (eth-out eth-out-port)
		    (p packet))
  (assert (obj= p (first (to-eth-of ap))) nil
	  "leaving ap eth-out p: not the first packet to-eth!")
  (pop (to-eth-of ap)))


(defmethod leaving ((ap access-point) (wlan-out wlan-out-port)
		    (p packet))
  (assert (obj= p (first (to-wlan-of ap))) nil
	  "leaving ap wlan-out p: not the first packet to-wlan!")
  (pop (to-wlan-of ap)))




(defmethod output ((ap access-point) (out out-port) (obj object))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))




(defmethod send-to-wlan ((ap access-point))
  (with-accessors ((to-wlan to-wlan-of)
		   (clock clock-of)
		   (wlan-out wlan-out-of)) ap
    (assert to-wlan nil
	    "send-to-wlan has nothing to send!")
    (list (make-instance 'event
			 :time clock
			 :owner ap
			 :fn #'output
			 :args (list wlan-out
				     (first to-wlan))))))


(defmethod send-to-eth ((ap access-point))
  (with-accessors ((to-eth to-eth-of)
		   (clock clock-of)
		   (eth-out eth-out-of)) ap
    (assert to-eth nil
	    "send-to-eth has nothing to send!")
    (list (make-instance 'event
			 :time clock
			 :owner ap
			 :fn #'output
			 :args (list eth-out
				     (first to-eth))))))
