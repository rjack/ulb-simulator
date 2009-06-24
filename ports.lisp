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


;; OVERVIEW

;; TODO


(in-package :ulb-sim)



(defclass voice-in-port (in-port)
  nil)


(defclass voice-out-port (out-port)
  nil)


(defclass packet-in-port (in-port)
  nil)

(defclass packet-out-port (out-port)
  nil)


(defclass udp-in-port (packet-in-port)
  nil)


(defclass udp-out-port (packet-out-port)
  nil)


(defclass rtp-in-port (packet-in-port)
  nil)


(defclass rtp-out-port (packet-out-port)
  nil)


(defclass wifi-frame-in-port (packet-in-port)
  nil)


(defclass wifi-frame-out-port (packet-out-port)
  nil)


(defclass data-in-port (packet-in-port)
  nil)


(defclass data-out-port (packet-out-port)
  nil)


(defmethod lock-port ((sim simulator) (voice-out voice-out-port)
		      (vo voice))
  "Voice-out locked until sim finishes talking."
  (list (make-instance 'event
		       :owner sim
		       :time (+ (clock-of sim)
				(duration-of vo))
		       :fn #'unlock-port
		       :args (list voice-out))))
