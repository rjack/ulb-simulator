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
;;   phone-in-port  --> +-----+ --> wlan0-out-port
;;                      | ULB |       WLAN0
;;                      |     | <-- wlan0-in-port
;;       SOFTPHONE      |     |
;;                      |     | --> wlan1-out-port
;;                      |     |       WLAN1
;;   phone-out-port <-- +-----+ <-- wlan0-in-port


(in-package :ulb-sim)


(defclass wlan-in-port (udp-in-port)
  nil)


(defclass wlan-out-port (udp-in-port)
  ((state
    :initarg :working
    :accessor state-of
    :type keyword)))


(defclass ted-in-port (netlink-in-port)
  nil)


(defclass iface-status ()
  ((value
    :initform :working
    :accessor value-of
    :type (member :working :suspected))))


(defclass ulb (simulator)
  ((phone-in
    :accessor phone-in-of
    :type phone-in-port)
   (phone-out
    :accessor phone-out-of
    :type phone-out-port)
   (ted0-in
    :accessor ted0-in-of
    :type ted-in-port)
   (wlan0-status
    :initform (make-instance 'iface-status)
    :accessor wlan0-status-of
    :type iface-status)
   (wlan1-status
    :initform (make-instance 'iface-status)
    :accessor wlan1-status-of
    :type iface-status)
   (wlan0-in
    :accessor wlan0-in-of
    :type wlan-in-port)
   (wlan0-out
    :accessor wlan0-out-of
    :type wlan-out-port)
   (wlan1-in
    :accessor wlan1-in-of
    :type wlan-in-port)
   (wlan1-out
    :accessor wlan1-out-of
    :type wlan-out-port)
   (ted1-in
    :accessor ted1-in-of
    :type ted-in-port)
   (sent
    :initform (list)
    :accessor sent-of
    :type list)
   (to-wlan
    :initform (list)
    :accessor to-wlan-of
    :type list)
   (to-wlan-urg
    :initform (list)
    :accessor to-wlan-urg-of
    :type list)
   (to-phone
    :initform (list)
    :accessor to-phone-of
    :type list)))


(defmethod in-ready-evs ((u ulb) (wlan-out wlan-out-port))
  (send-datagram-evs u))

(defmethod out-ready-evs ((u ulb) (wlan-out wlan-out-port))
  (send-datagram-evs u))

(defmethod in-ready-evs ((u ulb) (phone-out phone-out-port))
  (send-to-phone-evs u))

(defmethod out-ready-evs ((u ulb) (phone-out phone-out-port))
  (send-to-phone-evs u))



(defmethod receive ((u ulb) (phone-in phone-in-port)
		    (rtp rtp-packet))
  "Contract: ulb phone-in-port rtp-packet -> end-of-life-event

   Side effects: encapsulate rtp in a ulb-dgram-struct and enqueue into
                 to-wlan."
  (with-accessors ((to-wlan to-wlan-of)) u
    (let ((eol-ev (fresh-eol-event)))
      (setf to-wlan
	    (append to-wlan
		    (list (make-instance 'ulb-dgram-struct
					 :eol-ev eol-ev
					 :data rtp))))
      eol-ev)))


(defmethod first-to-send ((u ulb))
  (or (first (to-wlan-urg-of u))
      (first (to-wlan-of u))))


(defmethod best-out-port ((u ulb))
  (if (eql :working (value-of (wlan0-status-of w)))
      (wlan0-out-of u)
      (wlan1-out-of u)))


(defmethod input-evs ((u ulb) (phone-in phone-in-port)
		      (rtp rtp-packet))
  "From phone"
  (call-next-method)
  (let ((must-send-p (null (first-to-send u))))
    (cons (the event (receive u phone-in rtp))
	  (when must-send-p
	    (send-datagram-evs u)))))


(defmethod input-evs ((u ulb) (wlan-in wlan-in-port)
		      (udp udp-packet))
  "From wlan"
  (call-next-method)
  ;; TODO set working
  (let ((must-send-p (null (to-phone-of u))))
    (receive u wlan-in udp)
    (when must-send-p
      (send-to-phone-evs u))))


(defmethod input-evs ((u ulb) (wlan-notify-in wlan-notify-in-port)
		      (ntf netlink-packet))
  "From wlan TED"
  (remove-child u ntf)
  (let ((id (car (payload-of ntf)))
	(msg (cdr (payload-of ntf))))
    (if (eql :ack msg)
	(the null (discard u id))
	(send-again u id))))


(defmethod output ((u ulb) (out out-port) (obj object))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))


(defmethod get-identification ((uds ulb-struct-datagram))
  (identification-of (payload-of uds)))


(defmethod discard ((u ulb) id)
  (declare (id id-type))
    (let ((target (find id :key #'get-identification
			(append (sent-of u)
				(to-wlan-of u)
				(to-wlan-urg-of u)))))
      (when target
	(setf (sent-of u)
	      (remove target (sent-of u)))
	(setf (to-wlan-of u)
	      (remove target (to-wlan-of u)))
	(setf (to-wlan-urg-of u)
	      (remove target (to-wlan-urg-of u))))))
  nil)


(defmethod send-again ((u ulb) id)
  (declare (id id-type))
  (let ((target (find id :key #'get-identification
		      (sent-of u))))
    (when target
      ;; remove from sent and enqueue to urgent
      (setf (sent-of u)
	    (remove target (sent-of u)))
      (setf (to-wlan-urg-of u)
	    (nconc (to-wlan-urg-of u)
		   (list target)))
      (send-datagram-evs u))))



(let ((sendmsg-id 0))

  (defmethod send-datagram-evs ((u ulb))
    (let ((next-to-send (first-to-send u))
	  (best-out (best-out-port u)))
      (when netx-to-send
	;; set sendmsg id
	(let ((udp (the udp-packet (payload-of uds))))
	  (when (not (slot-boundp udp 'identification))
	    (setf (identification-of udp)
		  (incf sendmsg-id))))
	;; enqueue
	(setf (sent-of u)
	      (append (sent-of u)
		      (list uds)))
	;; output event
	(list (make-instance 'event
			     :owner u
			     :time (clock-of u)
			     :fn #'output
			     :args (list best-out
					 (clone (packet-of uds)))))))))


(defmethod post-output-evs ((u ulb) (wlan-out wlan-out-port)
			    (pkt packet))
  (send-datagram-evs u))


(defmethod post-output-evs ((u ulb) (phone-out phone-out-port)
			    (pkt packet))
  (send-to-phone-evs u))
