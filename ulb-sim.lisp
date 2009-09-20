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


;; Bag generiche

(defclass out-bag (bag)
  nil)
(defclass in-bag (bag)
  nil)
(defclass out-fbag (fbag)
  nil)
(defclass in-fbag (fbag)
  nil)


;; Definizioni per sphone-sim.

(defclass sphone-sim (sim)
  ((out   :initarg :out :accessor out :type out-fbag)
   (in    :initarg :in  :accessor in  :type in-bag)))


(defmethod setup-new! ((ss sphone-sim))
  (with-slots (out in) ss
    (setf out (new 'out-fbag :owner ss))
    (setf in (new 'in-bag :owner ss)))
  (call-next-method))


;; Definizioni per ulb-sim e derivati.

(defclass ulb-out-fbag (fbag)
  nil)

(defclass ulb-sent-bag (bag)
  nil)


(defclass ulb-wlan-out-bag (fbag)
  ((fw-capab    :initarg :fw-capa     :accessor fw-capa     :documentation "Capacita' di notifica del firmware: ack, nack o entrambi")
   (err-no      :initarg :err-no      :accessor err-no      :documentation "Numero di errori di invio")
   (max-err-no  :initarg :max-err-no  :accessor max-err-no  :documentation "Massimo numero di errori d'invio")
   (retry-tmout :initarg :retry-tmout :accessor retry-tmout :documentation "Tempo di attesa prima di riprovare a inviare il frame")
   (retry-event :initarg :retry-event :accessor retry-event :documentation "Riferimento all'evento schedulato per re-invio frame")))


(defmethod setup-new! ((wob ulb-wlan-out-bag))
  (with-slots (err-no max-err-no retry-tmout retry-event) wob
    (setf err-no 0)
    (setf max-err-no 7)            ; valore preso dal paper di ghini
    (setf retry-tmout (usecs 3))   ; http://www.air-stream.org.au/ACK_Timeouts dice 2, ma il retry-event viene schedulato prima
    (setf retry-event nil))
  (call-next-method))


(defclass ulb-wlan-in-fbag (fbag)
  nil)


(defclass ulb-sim (sim)
  ((sendmsg-id :initarg :sendmsg-id :accessor sendmsg-id)
   (out        :initarg :out        :accessor out    :type ulb-out-fbag)
   (in         :initarg :in         :accessor in     :type in-fbag)
   (sent       :initarg :sent       :accessor sent   :type ulb-sent-bag)
   (w0-out     :initarg :w0-out     :accessor w0-out :type ulb-wlan-out-bag)
   (w0-in      :initarg :w0-in      :accessor w0-in  :type ulb-wlan-in-fbag)
   (w1-out     :initarg :w1-out     :accessor w1-out :type ulb-wlan-out-bag)
   (w1-in      :initarg :w1-in      :accessor w1-in  :type ulb-wlan-in-fbag)))


(defclass ulb-dummy-sim (ulb-sim)
  nil)

(defclass ulb-simple-sim (ulb-sim)
  nil)

(defclass ulb-stoca-sim (ulb-sim)
  nil)


;; METODI ULB-SIM

(defmethod setup-new! ((us ulb-sim))
  (with-slots (sendmsg-id out in w0-out w0-in w1-out w1-in sent) us
    (setf sendmsg-id -1)
    (setf out    (new 'ulb-out-fbag     :owner us))
    (setf in     (new 'in-fbag          :owner us))
    (setf sent   (new 'ulb-sent-bag     :owner us))
    (setf w0-out (new 'ulb-wlan-out-bag :owner us))
    (setf w0-in  (new 'ulb-wlan-in-fbag :owner us))
    (setf w1-out (new 'ulb-wlan-out-bag :owner us))
    (setf w1-in  (new 'ulb-wlan-in-fbag :owner us))
    (connect! out w0-out)
    (connect! w0-out sent)
    (connect! w0-in in)
    (connect! out w1-out)
    (connect! w1-out sent)
    (connect! w1-in in)
    (connect! sent out))
  (call-next-method))


(defmethod sendmsg-getid ((us ulb-sim))
  (incf (sendmsg-id us)))


;; METODI ULB-DUMMY-SIM

;; praticamente tutti i default vanno bene?
;; le wlan devono scartare gli ack degli access point quando li
;; ricevono.


;; METODI ULB-STOCA-SIM

;; ULB-OUT-FBAG

(defmethod in! ((us ulb-stoca-sim) (uob ulb-out-fbag) (rp rtp-packet)
		dst-bag dst-sim)
  "ULB-STOCA riceve rtp da sphone, lo mette in una struct e imposta il
   tstamp di arrivo"
  (let ((rs (new 'rtp-struct :pkt rp :tstamp (gettime!))))
    (call-next-method us uob rs)))


(defmethod insert! ((uob ulb-out-fbag) (rs rtp-struct) &key)
  "Dentro a out le rtp-struct sono in ordine ascendente di timestamp"
  (call-next-method)
  (! (setf (elements uob)
	   (stable-sort (elements uob) #'< :key #'tstamp))))


(defmethod remove! ((uob ulb-out-fbag) &key)
  "Se un rtp-struct e' piu' vecchia di 150 ms viene scartata."
  (let ((rs (call-next-method uob)))
    (if (>= (- (gettime!) (tstamp rs))
	    (msecs 150))
	(remove! uob)   ; questo scartato, riprova
	rs)))


(defmethod out! ((us ulb-stoca-sim) (uob ulb-out-fbag)
		 dst-bag dst-sim)
  "out-fbag -> best wlan"
  ;; TODO scegliere best-wlan in modo piu' rigoroso.
  (handler-bind ((access-temporarily-unavailable #'abort)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (let ((best-wlan (random-pick (dests uob))))
      (call-next-method us uob best-wlan (owner best-wlan)))))


;; WLAN-OUT-BAG

(defmethod default-dest ((us ulb-stoca-sim) (wob ulb-wlan-out-bag))
  "Destinazione di default per le wlan e' il link"
  (let ((ln-bag (find-if (lambda (d)
			   (not (typep d 'ulb-sent-bag)))
			 (dests wob))))
    (if (null ln-bag)
	(error 'no-destination)
	ln-bag)))


(defmethod in! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag) (rs rtp-struct)
		dst-bag dst-sim)
  (when (not (empty? wob))
    (error "in! wlan non e' vuota!"))
  (lock! wob)
  (call-next-method)
  ;; a questo punto wob e' lockata, ha una sola rtp-struct e la
  ;; inoltra al link wifi (uso t t perche' il link e' dest di default)
  (schedule! (new 'event :owner-id (id us)
		  :desc (format nil "out! ~a ~a ~a ~a" us wob t t)
		  :tm (next-out-time us wob)
		  :fn (lambda ()
			(out! us wob t t)))))


(defmethod give-up! ((wob ulb-wlan-out-bag))
  ;; reset wob
  ;; notifica TED se il firmware e' in grado di notificare NACK
  (error 'not-implemented))


(defmethod handle-send-err! ((wob ulb-wlan-out-bag))
  (incf (err-no wob))
  (if (< (err-no wob)
	 (max-err-no wob))
      (out! (owner wob) wob t t)    ; riprova
      (give-up! wob)))


(defmethod out! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag)
		 (dst-bag bag) (dst-sim ln<->))
  "wlan -> wifi-link"
  ;; schedula l'in! per la destinazione
  ;; schedula il retry event
  ;; schedula l'out! per la bag sent, solo se siamo al primo invio
  ;; TODO LOG
  (assert (< (err-no wob)
	     (max-err-no wob))
	  nil "out! wob: oltrepassato numero massimo di invii!")
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method))
  (let ((retry-ev (new 'event :tm (+ (gettime!) (retry-tmout wob))
		       :desc (format nil "retry timeout expired ~a" wob)
		       :owner-id (id wob)
		       :fn (lambda ()
			     (handle-send-err! wob)))))
    (assert (or (null (retry-event wob))
		(dead? (retry-event wob)))
	    nil "out! wob: l'evento precedente e' ancora attivo!")
    (setf (retry-event wob) retry-ev)
    (schedule! retry-ev)
    (when (zerop (err-no wob))   ; solo al primo tentativo
      (schedule! (new 'event :tm (gettime!)
		      :desc "Invio pkt tra quelli spediti"
		      :owner-if (id wob)
		      :fn (lambda ()
			    (out! us wob (sent us) us)))))))


(defmethod out! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag)
		 (dst-bag ulb-sent-bag) (dst-sim ulb-stoca-sim))
  "wlan -> sent-bag"
  (error 'not-implemented))


(defmethod remove! ((wob ulb-wlan-out-bag) &key)
  (let ((qty (length (elements wob))))
    (if (not (= 1 qty))
	(error "remove! ulb-wlan-out-bag: ~a elementi invece che solo 1!" qty)
	(clone (first (elements wob))))))



;; METODI SPHONE-SIM

;; TODO conversazione: lista di eventi in! pregenerata che inietta gli
;; rtp-packet nel softphone.

(defmethod out! ((ss sphone-sim) (ob out-fbag)
		 dst-bag dst-sim)
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method)))


(defmethod in! ((ss sphone-sim) (ib in-fbag) (rp rtp-packet)
		dst-bag dst-sim)
  (call-next-method))
