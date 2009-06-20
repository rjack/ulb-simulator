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
;;     voice-in-port  --> +-----------+ --> udp-out-port
;;                        | SOFTPHONE |
;;     voice-out-port <-- +-----------+ <-- udp-in-port


(in-package :ulb-sim)


(defclass softphone (simulator)
  ((outgoing-packets
    :initargs :outgoing-packets
    :initform (list)
    :accessor outgoing-packets-of
    :type list
    :documentation "List of udp-packets to be sent to ULB")
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
   (udp-in
    :accessor udp-in-of
    :type udp-in-port)
   (udp-out
    :accessor udp-out-of
    :type udp-out-port)))



(defmethod remove-child ((sp softphone) (vo voice))
  (with-accessors ((outgoing-voices outgoing-voices-of)) sp
    (if (eq vo (first outgoing-voices))
	(pop outgoing-voices)
	(assert (null (find vo outgoing-voices)) nil
		"Removing the wrong child"))
    (call-next-method)))


(defmethod remove-child ((sp softphone) (up udp-packet))
  (with-accessors ((outgoing-packets outgoing-packets-of)) sp
    (if (eq up (first outgoing-packets))
	(pop outgoing-packets)
	(assert (null (find up outgoing-packets)) nil
		"Removing the wrong child"))
    (call-next-method)))


(let ((udp-min-size (bytes 300))
      (udp-max-size (bytes 700)))

  (defun random-packet-size ()
    (+ udp-min-size
       (random (1+ (- udp-max-size udp-min-size))))))



(let ((codec-bw (kilobytes-per-second 16)))

  (defmethod voice->udp-packets ((sp softphone) (vo voice))
    (labels ((packets (nbytes-left list-acc)
	       (if (zerop nbytes-left)
		   list-acc
		   (let ((size (min nbytes-left
				    (random-packet-size))))
		     (packets (- nbytes-left size)
			      (cons (make-instance 'udp-packet
						   :size size)
				    list-acc))))))
      (remove-child sp vo)
      (let ((nbytes (* codec-bw (duration-of vo))))
	(nreverse (packets nbytes (list))))))


  (defmethod udp-packet->voice ((sp softphone) (up udp-packet))
    (remove-child sp up)
    (make-instance 'voice :duration (/ (size-of up)
				       codec-bw))))


(defmethod handle-input ((sp softphone) (in voice-in-port) (vo voice))
  "Voice from user is converted in udp-packets."
  (call-next-method)
  (with-accessors ((outgoing-packets outgoing-packets-of)) sp
    (let ((must-start-p (null outgoing-packets)))
      (setf outgoing-packets (append outgoing-packets
				     (voice->udp-packets sp vo)))
      (when must-start-p
	(send-to-ulb sp)))))



(defmethod handle-input ((sp softphone) (in udp-in-port)
			 (up udp-packet))
  (call-next-method)
  (with-accessors ((outgoing-voices outgoing-voices-of)) sp
    (let ((must-start-p (null outgoing-voices)))
      (setf outgoing-voices (append outgoing-voices
				    (udp-packet->voice sp up)))
      (when must-start-p
	(send-to-user sp)))))


(defmethod port-ready ((sp softphone) (voice-out voice-out-port))
  (when (outgoing-voices-of sp)
    (send-to-user sp)))


(defmethod send-to-ulb ((sp softphone))
  (with-accessors ((outgoing-packets outgoing-packets-of)) sp
    (assert (not (null outgoing-packets)) nil
	    "send-to-ulb has nothing to send!")
    (list (make-instance 'event
			 :time (clock-of sp)
			 :owner sp
			 :fn #'output
			 :args (list (udp-out-of sp)
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
