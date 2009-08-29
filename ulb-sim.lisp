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


(defmethod setup-new ((us ulb-sim))
  (let ((links (list (ln-> 'lo 'outq)
		     (ln<=> 'outq 'wlan0)
		     (ln<=> 'outq 'wlan1)
		     (ln-> 'wlan0 'inq)
		     (ln-> 'wlan1 'inq)
		     (ln-> 'inq 'lo))))
    (with-slots (outq inq lo wlan0 wlan1 lm) us
      (setf outq  (new 'priority-queue))
      (setf inq   (new 'priority-queue))
      (setf lo    (new 'socket))
      (setf wlan0 (new 'socket))
      (setf wlan1 (new 'socket))
      (setf lm    (new 'link-manager :links links))
      us)))


;;; Metodo `IN'

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
			 :fn (lambda (sim)
			       (out sim 'lo rps)))
		    (new 'event               ; qui crea evento unlock
			 :owner-id id
			 :tm (+ tm
				(transfer-time (size rp)
					       (bandwidth-in lo)))
			 :fn (lambda (sim)
			       (unlock sim 'lo))))))))

;; Metodi `in' e `out' dovrebbero funzionare sia tra componenti
;; interni di un simulatore, sia tra diversi simulatori.

;; Tra la chiamata di `out' e quella di `in' dovrebbe esserci una
;; chiamata implicita `access' che controlla se il socket e' connesso
;; e senza lock.

;;; Metodo `OUT'

;; PROBLEMI:

;; Ora come ora, la connessione 'lo -> 'outq e' *implicita* e
;; *hard-coded* nel metodo `out' specializzato su 'lo.
;; NO! deve esserci un `lookup'.

;; Bisogna tentare di accedere alla porta, l'evento `in' deve essere
;; creato solo se l'accesso e' riuscito.
;; Due strade:
;; * `out', invece di chiamare direttamente `in', chiama `access' ed
;;   e' quest'ultima a chiamare `in' se non ci sono errori.
;; * `access' e' un metodo around di `in' che chiama
;;    (call-next-method) solo se non ci sono errori: in questo modo
;;    `out' puo' chiamare `in' direttamente e l'accesso e'
;;    trasparente.
;; TENTO LA PRIMA: piu' esplicita.
(defmethod out ((us ulb-sim) (slot (eql 'lo)) (rps rtp-struct))
  (with-slots (id lo tm) us
    (values us
	    (list (new 'event
		       :owner-id id :tm tm
		       :fn (lambda (sim)
			     ;; TODO: riesco a specificare i restart
			     ;; come &keys? Tipo
			     ;; (access sim 'outq rps
			     ;;         :locked #'wait
			     ;;         :not-connected #'abort)
			     (handler-bind ((locked #'wait)
					    (not-connected #'abort))
			       (access sim 'outq rps))))))))
