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


(defclass ulb-sim (sim)
  ((outq  :initarg :outq  :type bag)
   (inq   :initarg :inq   :type bag)
   (sent  :initarg :sent  :type bag)
   (wlan0 :initarg :wlan0 :type bag)
   (wlan1 :initarg :wlan1 :type bag)))


(defmethod setup-new ((us ulb-sim))
  (with-slots (outq inq wlan0 wlan1 sent)
    (setf outq  (new 'bag :owner us))
    (setf inq   (new 'bag :owner us))
    (setf sent  (new 'bag :owner us))
    (setf wlan0 (new 'bag :owner us))
    (setf wlan1 (new 'bag :owner us))
    (connect outq wlan0)
    (connect wlan0 sent)
    (connect wlan0 inq)
    (connect outq wlan1)
    (connect wlan1 sent)
    (connect wlan1 inq)
    (connect sent outq))
  (call-next-method))


;;; Metodo `IN'

;; Argomenti: sim elem object
(defmethod in ((us ulb-sim) (oq outq-el) (rp rtp-packet))
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

(defmethod out ((us ulb-sim) (slot (eql 'lo)))
  (handler-bind ((locked #'wait)
		 (not-connected #'abort))
    (access-link sim 'lo rps)))



;; `out' tra simulatori diversi: da ulb-sim ad access-point
(defmethod out ((us ulb-sim) (slot (eql 'wlan0)) (rps rtp-struct))
  (handler-bind ((locked #'wait)
		 (not-connected #'abort))
    (access-link sim 'wlan0 rps)))

(defmethod access-link ((us ulb-sim) (slot (eql 'wlan0)) (rps rtp-struct))
  ;; wlan0 e' collegata ad ap0.
  ;; il link che li collega e' nel link manager di scenario!
  ;; TODO come accedere?!
