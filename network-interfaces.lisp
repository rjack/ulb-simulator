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
;;     udp-in-port  --> +-------------------+ --> udp-out-port
;;        LINK          | NETWORK-INTERFACE |        HOST
;;     udp-out-port <-- +-------------------+ <-- udp-in-port
;;
;; handle-input udp-packet: output to other endpoint


(in-package :ulb-sim)


(defclass host-udp-in-port (udp-in-port)
  nil)


(defclass host-udp-out-port (udp-out-port)
  nil)


(defclass link-udp-in-port (udp-in-port)
  nil)


(defclass link-udp-out-port (udp-out-port)
  nil)


(defclass network-interface (simulator)
  ((to-link-packets
    :initform (list)
    :accessor to-link-packets-of
    :type list)
   (to-host-packets
    :initform (list)
    :accessor to-host-packets-of
    :type list)
   (link-udp-out
    :accessor link-udp-out-of
    :type link-udp-out-port)
   (link-udp-in
    :accessor link-udp-in-of
    :type link-udp-in-port)
   (host-udp-out
    :accessor host-udp-out-of
    :type host-udp-out-port)
   (host-udp-in
    :accessor host-udp-in-of
    :type host-udp-in-port)))


(defmethod handle-input ((ni network-interface)
			 (link-udp-in link-udp-in-port)
			 (up udp-packet))
  (call-next-method)
  (with-accessors ((to-host-packets to-host-packets-of)) ni
    (let ((must-start-p (null to-host-packets)))
      (setf to-host-packets (append to-host-packets
				    (list up)))
      (when must-start-p
	(send-to-host ni)))))


(defmethod handle-input ((ni network-interface)
			 (host-udp-in host-udp-in-port)
			 (up udp-packet))
  (call-next-method)
  (with-accessors ((to-link-packets to-link-packets-of)) ni
    (let ((must-start-p (null to-link-packets)))
      (setf to-link-packets (append to-link-packets
				    (list up)))
      (when must-start-p
	(send-to-link ni)))))


(defmethod port-ready ((ni network-interface)
		       (host-udp-out host-udp-out-port))
  (when (to-host-packets-of ni)
    (send-to-host ni)))


(defmethod port-ready ((ni network-interface)
		       (link-udp-out link-udp-out-port))
  (when (to-link-packets-of ni)
    (send-to-link ni)))


(defmethod send-to-host ((ni network-interface))
  (with-accessors ((to-host-packets to-host-packets-of)
		   (host-udp-out host-udp-out-of)
		   (clock clock-of)) ni
    (assert to-host-packets nil "send-to-host has nothing to send")
    (list (make-instance 'event
			 :time clock
			 :owner ni
			 :fn #'output
			 :args (list host-udp-out
				     (first to-host-packets))))))


(defmethod send-to-link ((ni network-interface))
  (with-accessors ((to-link-packets to-link-packets-of)
		   (link-udp-out link-udp-out-of)
		   (clock clock-of)) ni
    (assert to-link-packets nil "send-to-link has nothing to send")
    (list (make-instance 'event
			 :time clock
			 :owner ni
			 :fn #'output
			 :args (list link-udp-out
				     (first to-link-packets))))))


(defmethod remove-child ((ni network-interface) (up udp-packet))
  (with-accessors ((to-host-packets to-host-packets-of)
		   (to-link-packets to-link-packets-of)) ni
    (cond ((eq up (first to-host-packets))
	   (pop to-host-packets))
	  ((eq up (first to-link-packets))
	   (pop to-link-packets))
	  (t (error "removing the wrong child!")))))
