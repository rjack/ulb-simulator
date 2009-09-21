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



(defclass pkt (obj)
  ((hdr   :initarg :hdr :accessor hdr)
   (pld   :initarg :pld :accessor pld)))


(defclass rtp-pkt (pkt)
  nil)


(defmethod setup-new! ((rp rtp-pkt))
  (set-unbound-slots rp
    (hdr (bytes 12))))


(defclass udp-pkt (pkt)
  nil)

(defmethod setup-new! ((up udp-pkt))
  (set-unbound-slots up
    (hdr (bytes 8))))

(defclass wifi-frame (pkt)
  nil)

(defmethod size ((wf wifi-frame))
  (+ (hdr wf)
     (if (typep (pld wf) 'pkt)
	 (size (pld wf))
	 0)))   ; gli ack wifi hanno l'id che ackano come pld

(defmethod setup-new! ((wf wifi-frame))
  (set-unbound-slots wf
    (hdr (bytes 32))))


(defmethod size ((p pkt))
  (+ (hdr p)
     (size (pld p))))


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
  (set-unbound-slots ss
    (out (new 'out-fbag :owner ss))
    (in (new 'in-bag :owner ss)))
  (call-next-method))


;; Definizioni per ulb-sim e derivati.

(defclass ulb-out-fbag (fbag)
  nil)

(defclass ulb-sent-bag (bag)
  nil)


(defclass ulb-wlan-out-bag (fbag)
  ((fw              :initarg :fw              :accessor fw              :documentation "Capacita' reali di notifica del firmware: lista contenente :ack, :nack o entrambi")
   (fw-guess        :initarg :fw-guess        :accessor fw-guess        :documentation "Capacita' indovinate. Il gioco e' renderla uguale a fw, osservando il comportamento dell'interfaccia.")
   (mac-err-no      :initarg :mac-err-no      :accessor mac-err-no      :documentation "Numero attuale di errori di invio a livello MAC")
   (max-mac-err-no  :initarg :mac-max-err-no  :accessor max-mac-err-no  :documentation "Massimo numero di errori d'invio a livello MAC")
   (mac-retry-tmout :initarg :retry-tmout     :accessor retry-tmout     :documentation "Tempo di attesa prima di riprovare a inviare il frame")
   (mac-retry-event :initarg :retry-event     :accessor retry-event     :documentation "Riferimento all'evento schedulato per re-invio frame")
   (auto-nack-tmout :initarg :auto-nack-tmout :accessor auto-nack-tmout :documentation "Tempo di attesa entro cui se non viene notificato nulla si assume NACK")
   (auto-nack-event :initarg :auto-nack-event :accessor auto-nack-event :documentation "Riferimento all'evento per la notifica NACK automatica")))


(defmethod setup-new! ((wob ulb-wlan-out-bag))
  (set-unbound-slots wob
    (fw (random-pick (list (list :ack)
			   (list :nack)
			   (list :ack :nack))))
    (fw-guess (list))
    (mac-err-no 0)
    (max-mac-err-no 7)             ; valore preso dal paper di ghini
    (mac-retry-tmout (usecs 3))    ; http://www.air-stream.org.au/ACK_Timeouts dice 2, ma il retry-event verrebbe schedulato prima
    (mac-retry-event nil)
    (auto-nack-tmout (usecs 20))
    (auto-nack-event nil))
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
  (set-unbound-slots us
    (sendmsg-id -1)
    (out    (new 'ulb-out-fbag     :owner us))
    (in     (new 'in-fbag          :owner us))
    (sent   (new 'ulb-sent-bag     :owner us))
    (w0-out (new 'ulb-wlan-out-bag :owner us))
    (w0-in  (new 'ulb-wlan-in-fbag :owner us))
    (w1-out (new 'ulb-wlan-out-bag :owner us))
    (w1-in  (new 'ulb-wlan-in-fbag :owner us)))
  (with-slots (out in w0-out w0-in w1-out w1-in sent) us
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

(defmethod in! ((us ulb-stoca-sim) (uob ulb-out-fbag) (rp rtp-pkt)
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
  (handler-bind ((access-temporarily-unavailable #'abort)
		 (access-denied #'abort)
		 (no-destination #'abort))
    ;; TODO scegliere best-wlan in modo piu' rigoroso.
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
  "wlan riceve da out-bag: lock, crea la wifi frame e schedula invio."
  (when (not (clean? wob))
    (error "in! us wob rs t t: wob non e' clean"))
  (lock! wob)
  (let ((wf (new 'wifi-frame :pkt (new 'udp-packet :pkt (pkt rs)))))
    (call-next-method us wob wf)
    (schedule! (new 'event :owner-id (id us)
		    :desc (format nil "out! ~a ~a ~a ~a" us wob t t)
		    :tm (next-out-time us wob)
		    :fn (lambda ()
			  (out! us wob t t))))))


(defmethod give-up! ((wob ulb-wlan-out-bag))
  ;; reset wob
  ;; notifica TED se il firmware e' in grado di notificare NACK
  (error 'not-implemented))


(defmethod handle-send-err! ((wob ulb-wlan-out-bag))
;  (incf (err-no wob))
;  (if (< (err-no wob)
;	 (max-err-no wob))
;      (out! (owner wob) wob t t)    ; riprova
;      (give-up! wob)
  (error 'not-implemented))


(defmethod out! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag)
		 (dst-bag bag) (dst-sim ln<->))
  "wlan -> wifi-link"
  ;; schedula l'in! per la destinazione
  ;; schedula il retry event
  ;; schedula l'out! per la bag sent, solo se siamo al primo invio
  ;; TODO LOG
  (assert (< (mac-err-no wob)
	     (max-mac-err-no wob))
	  nil "out! wob: oltrepassato numero massimo di invii!")
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method))
  ;; TODO se non waiting!
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
    (when (zerop (mac-err-no wob))   ; solo al primo tentativo
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
;; rtp-pkt nel softphone.

(defmethod out! ((ss sphone-sim) (ob out-fbag)
		 dst-bag dst-sim)
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method)))


(defmethod in! ((ss sphone-sim) (ib in-fbag) (rp rtp-pkt)
		dst-bag dst-sim)
  (call-next-method))
