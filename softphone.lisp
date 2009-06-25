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
;;     voice-in-port  --> +-----------+ --> rtp-out-port
;;         PERSON         | SOFTPHONE |          ULB
;;     voice-out-port <-- +-----------+ <-- rtp-in-port


(in-package :ulb-sim)


(defclass softphone (simulator)
  ((to-ulb
    :initargs :to-ulb
    :initform (list)
    :accessor to-ulb-of
    :type list
    :documentation "List of rtp-packets to be sent to ULB")
   (to-person
    :initargs :to-person
    :initform (list)
    :accessor to-person-of
    :type list
    :documentation "List of voices to be sent to person.")
   (voice-in
    :accessor voice-in-of
    :type voice-in-port)
   (voice-out
    :accessor voice-out-of
    :type voice-out-port)
   (rtp-in
    :accessor rtp-in-of
    :type rtp-in-port)
   (rtp-out
    :accessor rtp-out-of
    :type rtp-out-port)))



(defmethod in-port-ready ((sp softphone) (voice-out voice-out-port))
  (when (to-person-of sp)
    (send-to-person sp)))

(defmethod out-port-ready ((sp softphone) (voice-out voice-out-port))
  (when (to-person-of sp)
    (send-to-person sp)))

(defmethod in-port-ready ((sp softphone) (rtp-out rtp-out-port))
  (when (to-ulb-of sp)
    (send-to-ulb sp)))

(defmethod out-port-ready ((sp softphone) (rtp-out rtp-out-port))
  (when (to-ulb-of sp)
    (send-to-ulb sp)))



(let ((codec-bw (kibibytes-per-second 16))
      (rtp-min-size (bytes 300))
      (rtp-max-size (bytes 700)))


  (defmethod receive ((sp softphone) (vo voice))
    (remove-child sp vo)
    (let ((rtp-packets (voice->rtp-packets vo
					   :bw codec-bw
					   :rtp-min rtp-min-size
					   :rtp-max rtp-max-size)))
      (with-accessors ((to-ulb to-ulb-of))
	  (add-children sp rtp-packets)
	(setf to-ulb
	      (append to-ulb
		      rtp-packets)))))


  (defmethod receive ((sp softphone) (rp rtp-packet))
    (remove-child sp rp)
    (let ((vo (rtp-packet->voice sp rp)))
      (with-accessors ((to-person to-person-of)) sp
	(setf to-person
	      (append to-person
		      (list vo)))))))



(defmethod handle-input ((sp softphone) (in voice-in-port) (vo voice))
  "Voice from person is converted in rtp-packets."
  (call-next-method)
  (let ((must-start-p (null (to-ulb-of sp))))
    (receive sp vo)
    (when must-start-p
      (send-to-ulb sp))))


(defmethod handle-input ((sp softphone) (in rtp-in-port)
			 (rp rtp-packet))
  (call-next-method)
  (let ((must-start-p (null (to-person-of sp))))
    (receive rp)
    (when must-start-p
      (send-to-person sp))))




(defmethod leaving ((sp softphone) (voice-out voice-out-port)
		    (vo voice))
  (with-accessors ((to-person to-person-of)) sp
    (assert (obj= vo (first to-person)) nil
	    "leaving sp voice-out vo: not the first voice to-person!")
    (pop to-person)))


(defmethod leaving ((sp softphone) (rtp-out rtp-out-port)
		    (rtp rtp-packet))
  (with-accessors ((to-ulb to-ulb-of)) sp
    (assert (obj= rtp (first to-ulb)) nil
	    "output sp rtp-out rtp: not the first rtp-packet to-ulb!")
    (pop to-ulb)))




(defmethod output ((sp softphone) (voice-out voice-out-port)
		   (vo voice))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'abort))
    (call-next-method)))


(defmethod output ((sp softphone) (rtp-out rtp-out-port)
		   (rtp rtp-packet))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))




(defmethod send-to-ulb ((sp softphone))
  (with-accessors ((to-ulb to-ulb-of)) sp
    (assert (not (null to-ulb)) nil
	    "send-to-ulb has nothing to send!")
    (list (make-instance 'event
			 :time (clock-of sp)
			 :owner sp
			 :fn #'output
			 :args (list (rtp-out-of sp)
				     (first to-ulb))))))


(defmethod send-to-person ((sp softphone))
  (with-accessors ((to-person to-person-of)) sp
    (assert (not (null to-person)) nil
	    "send-to-person has nothing to send!")
    (list (make-instance 'event
			 :time (clock-of sp)
			 :owner sp
			 :fn #'output
			 :args (list (voice-out-of sp)
				     (first to-person))))))
