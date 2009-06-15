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


(defclass softphone (actor)
  ((voice-in
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


;; schedulable
(defmethod handle-input ((sim simulator) (sp softphone) (evs list)
			 (in voice-in-port) (vo voice))
  "Voice from user is converted in udp-packets and sent to ULB"
  (labels ((output-event (udp-packet)
	     (make-instance 'event
			    :owner-path (path sp)
			    :time (gettime evs)
			    :fn #'do-output
			    :args (list (udp-out-of sp)
					udp-packet))))
    (schedule-all sim sp evs
		  (mapcar #'output-event
			  (voice->udp-packets vo)))))

(defmethod can-output ((sp softphone) (evs list) (out udp-out-port))



(defmethod do-output ((sp softphone) (evs list) (out udp-out-port)
		      (ud udp-packet))

  (put sim sp evs out ud))
