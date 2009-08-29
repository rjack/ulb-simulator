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


;; definizione di simulatore ULB
;; diventera' `defsim' un giorno?
;; NB: deve specificare solo la *conformazione* di un simulatore.
;;     `sim' sara' una classe di de-sim
;; TODO: la definizione dei collegamenti tra i componenti interni fa
;;       parte della conformazione o dello scenario?
(defclass ulb-sim (sim)
  ((outq  :initarg :outq  :type priority-queue)
   (inq   :initarg :inq   :type priority-queue)
   (lo    :initarg :lo    :type socket)
   (wlan0 :initarg :wlan0 :type socket)
   (wlan1 :initarg :wlan1 :type socket)))

;; Metodo `IN'
;; Argomenti: sim socket-name object
;; Note: la macro `with-locked-socket' dovrebbe impostare `lo' come
;; bloccata e aggiungere l'evento di unlocking agli eventi definiti
;; nel body.
(defmethod in ((us ulb-sim) (slot (eql 'lo)) (rp rtp-packet))
  (with-slots (id lo tm) us
    (let ((rps (new 'rtp-struct :pkt rp :tstamp tm)))
      ;; diventera' `with-locked-socket'?
      (values (the ulb-sim (lock us 'lo))     ; qui lock
	      (list (new 'event
			 :owner-id id :tm tm
			 :fn 'out :args '('lo rps))
		    (new 'event               ; qui crea evento unlock
			 :owner-id id
			 :tm (+ tm
				(transfer-time (size rp)
					       (bandwidth-in lo)))
			 :fn 'unlock :args '('lo)))))))

;; Metodi `in' e `out' dovrebbero funzionare sia tra componenti
;; interni di un simulatore, sia tra diversi simulatori.

;; Metodo `OUT'
;; Cosi' e' SBAGLIATO! Bisogna tentare di accedere alla porta,
;; l'evento `in' deve essere creato solo se l'accesso e' riuscito.
;; FIXME: dove inserisco l'handler-bind?
(defmethod out ((us ulb-sim) (slot (eql 'lo)) (rps rtp-struct))
  (with-slots (id lo tm) us
    (values us
	    (list (new 'event
		       :owner-id id :tm tm
		       :fn 'in :args '('outq rps))))))
