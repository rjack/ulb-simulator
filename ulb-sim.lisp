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

(defmethod size ((rp rtp-packet))
  ;; metodo farlocco, solo per test
  (mebibytes 1))

(defclass rtp-struct (obj)
  ((tstamp   :initarg :tstamp   :accessor tstamp)
   (pkt      :initarg :pkt      :accessor pkt)))


(defclass out-bag (bag)
  nil)
(defclass in-bag (bag)
  nil)
(defclass out-fbag (fbag)
  nil)
(defclass in-fbag (fbag)
  nil)
(defclass sent-bag (bag)
  nil)
(defclass wlan-fbag (fbag)
  nil)


(defclass sphone-sim (sim)
  ((out   :initarg :out :accessor out :type out-fbag)
   (in    :initarg :in  :accessor in  :type in-bag)))


(defmethod setup-new! ((ss sphone-sim))
  (with-slots (out in) ss
    (setf out (new 'out-fbag :owner ss))
    (setf in (new 'in-bag :owner ss)))
  (call-next-method ss))


(defclass ulb-sim (sim)
  ((out    :initarg :out    :type out-fbag)
   (in     :initarg :in     :type in-fbag)
   (sent   :initarg :sent   :type sent-bag)
   (w0-out :initarg :w0-out :type wlan-fbag)
   (w0-in  :initarg :w0-in  :type wlan-fbag)
   (w1-out :initarg :w1-out :type wlan-fbag)
   (w1-in  :initarg :w1-in  :type wlan-fbag)))


(defclass ulb-dummy-sim (ulb-sim)
  nil)

(defclass ulb-simple-sim (ulb-sim)
  nil)

(defclass ulb-stoca-sim (ulb-sim)
  nil)


;; METODI ULB-SIM

(defmethod setup-new! ((us ulb-sim))
  (with-slots (out in w0-out w0-in w1-out w1-in sent) us
    (setf out  (new 'out-fbag :owner us))
    (setf in   (new 'in-fbag :owner us))
    (setf sent  (new 'sent-bag :owner us))
    (setf w0-out (new 'wlan-fbag :owner us))
    (setf w0-in (new 'wlan-fbag :owner us))
    (setf w1-out (new 'wlan-fbag :owner us))
    (setf w1-in (new 'wlan-fbag :owner us))
    (connect! out w0-out)
    (connect! w0-out sent)
    (connect! w0-in in)
    (connect! out w1-out)
    (connect! w1-out sent)
    (connect! w1-in in)
    (connect! sent out))
  (call-next-method us))


;; METODI ULB-DUMMY-SIM

;; praticamente tutti i default vanno bene?
;; le wlan devono scartare gli ack degli access point.

;; METODI ULB-STOCA-SIM


(defmethod in! ((us ulb-stoca-sim) (ob out-fbag) (rp rtp-packet))
  (let ((rs (new 'rtp-struct :pkt rp :tstamp (gettime!))))
    (call-next-method us ob rp)))


(defmethod insert! ((ob out-fbag) (rs rtp-struct))
  "Inserisce e ordina le rtp-struct in ordine ascendente di timestamp"
  (call-next-method)
  (! (setf (elements ob)
	   (stable-sort (elements ob) #'< :key #'tstamp))))


(defmethod choose-dest ((us ulb-stoca-sim) (ob out-fbag)
			(rs rtp-struct))
  ;; TODO: analizzare i log di ogni interfaccia e assegnare un voto a ognuna:
  ;; scegliere quella col voto migliore.
  (random-pick (dests ob)))



;; METODI SPHONE-SIM

;; TODO conversazione: lista di eventi in! pregenerata che inietta gli
;; rtp-packet nel softphone.

(defmethod out! ((ss sphone-sim) (ob out-fbag))
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method ss ob)))


(defmethod in! ((ss sphone-sim) (ib in-fbag) (rp rtp-packet))
  (call-next-method ss ib rp))
