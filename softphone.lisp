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
;;     voice-in-port  --> +-----------+ --> packet-out-port
;;                        | SOFTPHONE |
;;     voice-out-port <-- +-----------+ <-- packet-in-port


(in-package :ulb-sim)


(defclass softphone (simulator)
  ((outgoing-packets
    :initargs :outgoing-packets
    :initform (list)
    :accessor outgoing-packets-of
    :type list
    :documentation "List of rtp-packets to be sent to ULB")
   (outgoing-voices
    :initargs :outgoing-voices
    :initform (list)
    :accessor outgoing-voices-of
    :type list
    :documentation "List of voices to be sent to user.")
   (voice-in
    :accessor voice-in-of
    :type voice-in-port)
   (voice-out
    :accessor voice-out-of
    :type voice-out-port)
   (packet-in
    :accessor packet-in-of
    :type packet-in-port)
   (packet-out
    :accessor packet-out-of
    :type packet-out-port)))



(let ((codec-bw (kibibytes-per-second 16))
      (rtp-min-size (bytes 300))
      (rtp-max-size (bytes 700)))


  (defmethod receive ((sp softphone) (vo voice))
    (remove-child sp vo)
    (let ((rtp-packets (voice->rtp-packets vo
					   :bw codec-bw
					   :rtp-min rtp-min-size
					   :rtp-max rtp-max-size)))
      (with-accessors ((outgoing-packets outgoing-packets-of))
	  (add-children sp rtp-packets)
	(setf outgoing-packets
	      (append outgoing-packets
		      rtp-packets)))))


  (defmethod receive ((sp softphone) (rp rtp-packet))
    (remove-child sp rp)
    (let ((vo (rtp-packet->voice sp rp)))
      (with-accessors ((outgoing-voices outgoing-voices-of)) sp
	(setf outgoing-voices
	      (append outgoing-voices
		      (list vo)))))))



(defmethod handle-input ((sp softphone) (in voice-in-port) (vo voice))
  "Voice from user is converted in rtp-packets."
  (call-next-method)
  (let ((must-start-p (null (outgoing-packets-of sp))))
    (receive sp vo)
    (when must-start-p
      (send-to-ulb sp))))


(defmethod handle-input ((sp softphone) (in packet-in-port)
			 (rp rtp-packet))
  (call-next-method)
  (let ((must-start-p (null (outgoing-voices-of sp))))
    (receive rp)
    (when must-start-p
      (send-to-user sp))))



(defmethod port-ready ((sp softphone) (voice-out voice-out-port))
  (when (outgoing-voices-of sp)
    (send-to-user sp)))


(defmethod port-ready ((sp softphone) (packet-out packet-out-port))
  (when (outgoing-packets-of sp)
    (send-to-ulb sp)))



(defmethod send-to-ulb ((sp softphone))
  (with-accessors ((outgoing-packets outgoing-packets-of)) sp
    (assert (not (null outgoing-packets)) nil
	    "send-to-ulb has nothing to send!")
    (list (make-instance 'event
			 :time (clock-of sp)
			 :owner sp
			 :fn #'output
			 :args (list (packet-out-of sp)
				     (first outgoing-packets))))))


(defmethod send-to-user ((sp softphone))
  (with-accessors ((outgoing-voices outgoing-voices-of)) sp
    (assert (not (null outgoing-voices)) nil
	    "send-to-user has nothing to send!")
    (list (make-instance 'event
			 :time (clock-of sp)
			 :owner sp
			 :fn #'output
			 :args (list (voice-out-of sp)
				     (first outgoing-voices))))))



(defmethod remove-child ((sp softphone) (vo voice))
  (with-accessors ((outgoing-voices outgoing-voices-of)) sp
    (if (eq vo (first outgoing-voices))
	(pop outgoing-voices)
	(assert (null (find vo outgoing-voices)) nil
		"Removing the wrong child"))
    (call-next-method)))


(defmethod remove-child ((sp softphone) (rp rtp-packet))
  (with-accessors ((outgoing-packets outgoing-packets-of)) sp
    (if (eq rp (first outgoing-packets))
	(pop outgoing-packets)
	(assert (null (find rp outgoing-packets)) nil
		"Removing the wrong child"))
    (call-next-method)))
