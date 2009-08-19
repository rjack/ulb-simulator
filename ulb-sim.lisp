;; ULB-SIM
;; UDP Load Balancing SIMulator.

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


(defpackage :it.unibo.cs.web.ritucci.ulb-sim
  (:nicknames :ulb-sim)
  (:use :common-lisp :de-sim)
  (:export))
(in-package :ulb-sim)


;; DEFSIM dice di cosa e' fatto un dato simulatore.
(defsim ulb
  outgoing (make-instance 'priority-queue)
  incoming (make-instance 'priority-queue)
  lo (make-instance 'channel)
  wlan0 (make-instance 'channel)
  wlan1 (make-instance 'channel))


;; DEFPATH definisce i percorsi degli oggetti che attraversano un dato
;; simulatore. Ovvero: *dove* vanno gli oggetti, e *come* passano da
;; una struttura dati all'altra?
(defpath ulb (rp rtp-packet)
  (lo :proc (make-instance 'rtp-struct :pkt rp)
	   :lock (transfer-time (size rp)
			     (bandwidth-in lo))
	   :flow t)
  (outgoing :flow t)
   (best wlan0 wlan1))
  (wlan0 incoming lo)
  (wlan1 incoming lo))

(defpath ulb netlink-packet
  (wlan0 :discard)
  (wlan1 :discard))
