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


;; Definizione di simulatore ULB
;; Diventera' `defsim' un giorno?
;; NB: deve specificare solo la *conformazione* di un simulatore.
;;     `sim' sara' una classe di de-sim
;; I collegamenti tra componenti interni vengono creati in `setup-new'

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


(defmethod children ((us ulb-sim))
  "Nessun simulatore interno, gli ulb-sim sono piatti"
  nil)


;;; Metodo `IN'

;; Argomenti: sim socket-name object
;; Note: la macro `with-locked-socket' dovrebbe impostare `lo' come
;; bloccata e aggiungere l'evento di unlocking agli eventi definiti
;; nel body.
(defmethod in ((us ulb-sim) (slot (eql 'lo)) (rp rtp-packet))
  (with-slots (id lo tm) us
    (let ((rps (new 'rtp-struct :pkt rp :tstamp tm)))
      ;; diventera' `with-locked-socket'?
      (values (the ulb-sim (! us :lo (busy (lo us))))  ; "lock"
	      (list (new 'event
			 :owner-id id :tm tm
			 :fn (lambda (sim)
			       (out sim 'lo rps)))
		    (new 'event                        ; "unlock"
			 :owner-id id
			 :tm (+ tm
				(transfer-time (size rp)
					       (bandwidth-in lo)))
			 :fn (lambda (sim)
			       (! sim :lo (idle (lo sim))))))))))

;; Metodi `in' e `out' dovrebbero funzionare sia tra componenti
;; interni di un simulatore, sia tra diversi simulatori.

;; Tra la chiamata di `out' e quella di `in' dovrebbe esserci una
;; chiamata implicita `access' che controlla se il socket e' connesso
;; e senza lock.
;; ANZI
;; `access' controlla che la *struttura dati* ricevente possa/voglia
;; ricevere l'oggetto. Non ci sono lock espliciti! Per esempio una
;; struttura dati di tipo `buffer' con una capacita' limitata puo'
;; sollevare un errore in una `access' perche' e' piena!
;; Quindi `locked' puo' essere un errore specifico come puo' esserlo
;; `full' o come `disabled' o mille altri.
;; Potrebbe esserci un errore di tipo `access-error' e tutti gli altri
;; essere sottoclassi di questo.
;; In questo modo il meccanismo di passaggio degli oggetti potrebbe
;; essere identico sia ai confini sia all'interno del simulatore.


;; Access per 'lo riesce solo se l'interfaccia e' inattiva, altrimenti
;; solleva l'errore `busy'.
;; PROBLEMA: questo come si rapporta ai vari link simplex, half-duplex
;; e duplex?
(defmethod access ((us ulb-sim) (slot (eql 'lo)) (rp rtp-packet))
  ;; FIXME! trasformami in access-link
  (if (idle-p (lo us))
      (in us 'lo rp)
      (error 'busy)))


;; Access per 'outq riesce sempre, quindi non solleva errori
(defmethod access ((us ulb-sim) (slot (eql 'outq)) (rps rtp-struct))
  ;; FIXME! trasformami in access-link
  (in us 'outq rps))


;;; Metodo `OUT'

;; `out', invece di chiamare direttamente `in', chiama `access-link'
;; che decide:
;; * se far uscire l'oggetto o se sollevare un errore `not-connected'
;;   o `locked', per cui l'oggetto manco lascia il simulatore.
;; * la destinazione
;; * quando e se l'oggetto arriva a destinazione
;; In definitiva `out' decide un'eventuale trasformazione dell'oggetto
;; e chiama `access-link' che fa il resto, ritornando gli eventi appropriati.

(defmethod out ((us ulb-sim) (slot (eql 'lo)) (rps rtp-struct))
  (handler-bind ((locked #'wait)
		 (not-connected #'abort))
    (access-link sim 'lo rps)))
