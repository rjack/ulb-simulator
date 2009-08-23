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


;; definizione di similatore ULB
;; diventera' `defsim' un giorno?
;; NB: deve specificare solo la *conformazione* di un simulatore.
(defclass ulb-sim (sim)
  ((outq  :initarg :outq  :type priority-queue)
   (inq   :initarg :inq   :type priority-queue)
   (lo    :initarg :lo    :type socket)
   (wlan0 :initarg :wlan0 :type socket)
   (wlan1 :initarg :wlan1 :type socket)))


(defin data-from-softphone ((u ulb) (:child lo)
			    (rp rtp-packet))


;; DEFSTEP definisce un passo di un percorso all'interno di un
;; simulatore.
(defstep (ulb lo)
    :datum (rp rtp-packet)
    :name "rtp-from-softphone"
    :lock (transfer-time (size rp)
			 (bandwidth-in lo))
  (let ((rps (make-instance 'rtp-struct :pkt rp
			                :tstamp (gettime ulb))))
    (next-step "rtp-outgoing" :datum rps)))


(defstep (ulb outgoing)
    :datum (rps rtp-struct)
    :name "rtp-outgoing"
    :lock 0
    (next-step (best (wlan0 ulb) (wlan1 ulb))
  (wlan0 incoming lo)
  (wlan1 incoming lo))

(defpath ulb netlink-packet
  (wlan0 :discard)
  (wlan1 :discard))
