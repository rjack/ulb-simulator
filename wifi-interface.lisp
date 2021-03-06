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
;;      link-in-port --> +----------------+ --> host-out-port
;;                       | WIFI-INTERFACE | --> host-notif-out-port
;;     link-out-port <-- +----------------+ <-- host-in-port
;;



(in-package :ulb-sim)



(defclass host-in-port (rtp-in-port)
  nil)


(defclass host-out-port (rtp-out-port)
  nil)


(defclass host-notif-out-port (netlink-out-port)
  nil)


(defclass link-in-port (wifi-frame-in-port)
  nil)


(defclass link-out-port (wifi-frame-out-port)
  nil)


(defclass wifi-interface (simulator)
  ((fw-cap
    :initarg :fw-cap
    :reader fw-cap-of
    :type list
    :documentation "(:ack :nak)")
   (to-link
    :initform (list)
    :accessor to-link-of
    :type list)
   (to-host
    :initform (list)
    :accessor to-host-of
    :type list)
   (to-host-notif
    :initform (list)
    :accessor to-host-notif-of
    :type list)
   (link-out
    :accessor link-out-of
    :type link-out-port)
   (link-in
    :accessor link-in-of
    :type link-in-port)
   (host-out
    :accessor host-out-of
    :type host-out-port)
   (host-in
    :accessor host-in-of
    :type host-in-port)
   (host-notif-out
    :accessor host-notif-out-of
    :type host-notif-out-port)
   (ack-timeout
    :accessor ack-timeout-of
    :type event)))



(defmethod ack-timeout-expired ((wi wifi-interface))
  (assert (to-link-of wi) nil
	  "Ack timeout expired, but nothing to re-send!")
  (nconc (when (find :nack (fw-cap-of wi))
	   (enqueue-notif wi :nak))
	 (send-to-link-evs wi)))


(defmethod lock-evs ((wi wifi-interface) (host-in host-in-port)
		      (p packet))
  "Stop and go algorithm! The port remains locked until the sent frame
   is acked back or the ack-timeout runs out."
  nil)




(defmethod in-ready-evs ((wi wifi-interface)
			  (host-out host-out-port))
  (when (to-host-of wi)
    (send-to-host-evs wi)))


(defmethod out-ready-evs ((wi wifi-interface)
			   (host-out host-out-port))
  (when (to-host-of wi)
    (send-to-host-evs wi)))


(defmethod in-ready-evs ((wi wifi-interface)
			  (link-out link-out-port))
  (when (to-link-of wi)
    (send-to-link-evs wi)))


(defmethod out-ready-evs ((wi wifi-interface)
			   (link-out link-out-port))
  (when (to-link-of wi)
    (send-to-link-evs wi)))


(defmethod out-ready-evs ((wi wifi-interface)
			   (host-notif-out host-notif-out-port))
  (when (to-host-notif-of wi)
    (notify-host wi)))


(defmethod in-ready-evs ((wi wifi-interface)
			  (host-notif-out host-notif-out-port))
  (when (to-host-notif-of wi)
    (notify-host wi)))


(defmethod enqueue-notif ((wi wifi-interface) notif)
  (declare (keyword notif))
  (with-accessors ((to-host-notif to-host-notif-of)) wi
    (setf to-host-notif
	  (append to-host-notif
		  (list notif)))))



(defmethod receive ((wi wifi-interface) (wf wifi-frame))
  (remove-child wi wf)
  (let ((rtp (the rtp-packet
	       (payload-of (the udp-packet
			     (payload-of wf))))))
    (with-accessors ((to-host to-host-of)) wi
      (setf to-host
	    (append to-host
		    (list rtp))))))


(defmethod receive ((wi wifi-interface) (rtp rtp-packet))
  (remove-child wi rtp)
  (let ((wf (udp-packet->wifi-frame
	     (rtp-packet->udp-packet rtp :src (id-of wi) :dst nil))))
    (with-accessors ((to-link to-link-of)) wi
      (setf to-link
	    (append to-link
		    (list wf))))))




(defmethod input-evs ((wi wifi-interface)
			 (link-in link-in-port)
			 (wf wifi-frame))
  (call-next-method)
  (let ((must-start-p (null (to-host-of wi))))
    (receive wi wf)
    (when must-start-p
      (send-to-host-evs wi))))


(defmethod input-evs ((wi wifi-interface)
			 (link-in link-in-port)
			 (ack wifi-ack-frame))
  "Stop and go: when receiving an ack, cancel the ack-timeout and
   unlock the host-in-port. If wi can ack, notify it."
  (call-next-method)
  (remove-child wi ack)
  (cancel-event (ack-timeout-of wi))
  (let ((unlock-event (make-instance 'event
				     :owner wi
				     :time (clock-of wi)
				     :fn #'unlock-evs
				     :args (list (host-in-of wi)))))
    (cons unlock-event
	  (when (find :ack (fw-cap-of wi))
	    (enqueue-notif wi :ack)))))


(defmethod input-evs ((wi wifi-interface)
			 (host-in host-in-port)
			 (rtp rtp-packet))
  (call-next-method)
  (let ((must-start-p (null (to-link-of wi))))
    (receive wi rtp)
    (when must-start-p
      (send-to-link-evs wi))))




(defmethod post-output-evs ((wi wifi-interface)
			    (link-out link-out-port)
			    (wf wifi-frame))
  "Frame has been sent: set the ack timeout and don't remove the local
   copy of wf, since it may be needed if the sent one will not be
   acked."
  (assert (eql (size wf)
	       (size (first (to-link-of wi))))
	  nil
	  "output wi link-out wf: not the first wifi frame to-host!")
  (let ((ack-tmout (make-instance 'event
				  :owner wi
				  :time (+ (clock-of wi)
					   (usecs 300))
				  :fn #'ack-timeout-expired
				  :args (host-in-of wi))))
    (setf (ack-timeout-of wi)
	  ack-tmout)
    (cons ack-tmout
	  (when (to-link-of wi)
	    (send-to-link-evs wi)))))


(defmethod post-output-evs ((wi wifi-interface)
			    (host-out host-out-port)
			    (rtp rtp-packet))
  (assert (obj= rtp (first (to-host-of wi))) nil
	  "output wi host-out rtp: not the first rtp packet to-host!")
  (pop (to-host-of wi))
  (when (to-host-of wi)
    (send-to-host-evs wi)))


(defmethod output-evs ((wi wifi-interface)
		   (out out-port)
		   (obj object))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))


(defmethod notify-host ((wi wifi-interface))
  (with-accessors ((to-host-notif to-host-notif-of)
		   (host-notif-out host-notif-out-of)
		   (clock clock-of)) wi
    (list (make-instance 'event
			 :time clock
			 :owner wi
			 :fn #'output-evs
			 :args (list host-notif-out
				     (first to-host-notif))))))


(defmethod send-to-host-evs ((wi wifi-interface))
  (with-accessors ((to-host to-host-of)
		   (host-out host-out-of)
		   (clock clock-of)) wi
    (assert to-host nil "send-to-host-evs has nothing to send")
    (list (make-instance 'event
			 :time clock
			 :owner wi
			 :fn #'output-evs
			 :args (list host-out
				     (first to-host))))))


(defmethod send-to-link-evs ((wi wifi-interface))
  (with-accessors ((to-link to-link-of)
		   (link-out link-out-of)
		   (clock clock-of)) wi
    (assert to-link nil "send-to-link-evs has nothing to send")
    (list (make-instance 'event
			 :time clock
			 :owner wi
			 :fn #'output-evs
			 :args (list link-out
				     (clone (first to-link)))))))