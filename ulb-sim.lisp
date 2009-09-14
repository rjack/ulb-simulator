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


(defclass rtp-packet (obj)
  ;; TODO definire slot
  nil)

(defclass rtp-struct (obj)
  ((tstamp :initarg :tstamp :accessor tstamp)
   (pkt    :initarg :pkt    :accessor pkt)))

(defclass out-bag (bag)
  nil)
(defclass in-bag (bag)
  nil)
(defclass sent-bag (bag)
  nil)
(defclass wlan-bag (bag)
  nil)


(defclass sphone-sim (sim)
  ((outq  :initarg :outq  :type out-bag)
   (inq   :initarg :inq   :type in-bag)))


(defclass ulb-sim (sim)
  ((outq  :initarg :outq  :type out-bag)
   (inq   :initarg :inq   :type in-bag)
   (sent  :initarg :sent  :type sent-bag)
   (wlan0 :initarg :wlan0 :type wlan-bag)
   (wlan1 :initarg :wlan1 :type wlan-bag)))


(defmethod setup-new! ((us ulb-sim))
  (with-slots (outq inq wlan0 wlan1 sent) us
    (setf outq  (new 'out-bag :owner us))
    (setf inq   (new 'in-bag :owner us))
    (setf sent  (new 'sent-bag :owner us))
    (setf wlan0 (new 'wlan-bag :owner us))
    (setf wlan1 (new 'wlan-bag :owner us))
    (connect! outq wlan0)
    (connect! wlan0 sent)
    (connect! wlan0 inq)
    (connect! outq wlan1)
    (connect! wlan1 sent)
    (connect! wlan1 inq)
    (connect! sent outq))
  (call-next-method))


;; METODI ULB-SIM

(defmethod in! ((us ulb-sim) (ob out-bag) (rp rtp-packet))
  (let ((rps (new 'rtp-struct :pkt rp :tstamp (tm us))))
    (call-next-method us ob rps)))


(defmethod choose-dest ((us ulb-sim) (ob out-bag) (rs rtp-struct))
  "TODO: dest di ob sono wlan0 e wlan1, analizzo i log, calcolo
   punteggio e scelgo la migliore"
  nil)


;; Il metodo `out!' e' l'unico che genera eventi.
;; La sequenza di chiamate e':
;; - in! -> insert! -> flush!
;; - continue-flush! | start-flush!
;; - out! = nuovi eventi (in! e continue-flush!)
(defmethod out! ((us ulb-sim) (ob out-bag) (rp rtp-packet))
  (handler-bind ((access-temporarily-unavailable #'wait) ; access?
		 (access-denied #'abort)                 ; access?
		 (no-destination #'abort))               ; choose-dest
    (call-next-method us ob rp)))
