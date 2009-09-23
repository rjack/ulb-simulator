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


(defgeneric clean? (wob))
(defgeneric clean! (wob))
(defgeneric mac-retry! (wob))
(defgeneric give-up! (wob))
(defgeneric auto-nack! (wob))
(defgeneric sendmsg-getid! (us))
(defgeneric notify-nack! (us wob id))


(defclass pkt-log-entry (obj)
  ((tstamp        :initarg :tstamp         :accessor tstamp)
   (sendmsg-id    :initarg :sendmsg-id     :accessor sendmsg-id)
   (notification  :initarg :notification   :accessor notification)))

(defmethod setup-new! ((ple pkt-log-entry))
  (set-unbound-slots ple
    (tstamp       (error "pkt-log-entry: tstamp necessario!"))
    (sendmsg-id   (error "pkt-log-entry: sendmsg-id necessario!"))
    (notification (error "pkt-log-entry: notification necessaria!")))
  (call-next-method))


(defclass ping-log-entry (pkt-log-entry)
  ((reply-tstamp  :initarg :reply-tstamp   :accessor reply-tstamp)
   (ping-seqnum   :initarg :ping-seqnum    :accessor ping-seqnum)))

(defmethod setup-new! ((ple ping-log-entry))
  (set-unbound-slots ple
    (reply-tstamp (error "ping-log-entry: reply-tstamp necessario!"))
    (ping-seqnum  (error "ping-log-entry: notification necessaria!")))
  (call-next-method))


;; PACCHETTI

(defclass pkt (obj)
  ((hdr-size      :initarg :hdr-size       :accessor hdr-size)
   (pld           :initarg :pld            :accessor pld)))

(defmethod setup-new! ((p pkt))
  (set-unbound-slots p
    (hdr-size 0))
  (call-next-method))

(defmethod clone ((p pkt))
  (let ((copy (call-next-method)))
    (setf (pld copy)
	  (clone (pld p)))
    copy))

(defmethod size ((p pkt))
  (+ (hdr-size p)
     (size (pld p))))


(defclass data-pkt (pkt)
  ;; pld e' una stringa, la sua lunghezza e' la dimensione del pld
  nil)

(defmethod setup-new! ((dp data-pkt))
  (set-unbound-slots dp
    (pld ""))
  (call-next-method))

(defmethod clone ((str string))
  (copy-seq str))

(defmethod size ((str string))
  (length str))


(defclass dummy-data-pkt (data-pkt)
  ;; pld e' un numero che rappresenta la dimensione del payload
  nil)

(defmethod setup-new! ((ddp dummy-data-pkt))
  (set-unbound-slots ddp
    (pld 0))
  (call-next-method))

(defmethod clone ((n number))
  n)

(defmethod size ((n number))
  n)


(defclass rtp-pkt (pkt)
  nil)

(defmethod setup-new! ((rp rtp-pkt))
  (set-unbound-slots rp
    (hdr-size (bytes 12)))
  (call-next-method))

(defclass udp-pkt (pkt)
  nil)

(defmethod setup-new! ((up udp-pkt))
  (set-unbound-slots up
    (hdr-size (bytes 8)))
  (call-next-method))

(defclass wifi-frame (pkt)
  ((seq       :initarg :seq        :accessor seq)))

(defmethod setup-new! ((wf wifi-frame))
  (set-unbound-slots wf
    (hdr-size (bytes 32)))
  (call-next-method))



(defclass pkt-struct (obj)
  ((tstamp     :initarg :tstamp     :accessor tstamp)
   (sendmsg-id :initarg :sendmsg-id :accessor sendmsg-id)
   (pkt        :initarg :pkt        :accessor pkt :type pkt)))


(defmethod setup-new! ((ps pkt-struct))
  (set-unbound-slots ps
    (pkt (error "pkt-struct: pkt necessario!")))
  (call-next-method))


(defmethod clone ((ps pkt-struct))
  (let ((copy (call-next-method)))
    (setf (pkt copy)
	  (clone (pkt ps)))
    copy))


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


(defclass ulb-wlan-out-bag (bag)
  ((fw              :initarg :fw              :accessor fw              :documentation "Capacita' reali di notifica del firmware: lista contenente :ack, :nack o entrambi")
   ;; wib riceve mac-ack: aggiungo :ack
   ;; wib riceve mac-nack: aggiungo :nack
   ;; wib riceve un ping: se l'invio del ping corrispondente non e' stato mac-ackato, aggiungo :nack
   (fw-guess        :initarg :fw-guess        :accessor fw-guess        :documentation "Capacita' indovinate. Il gioco e' renderla uguale a fw, osservando il comportamento dell'interfaccia.")
   (pkt-struct      :initarg :pkt-struct      :accessor pkt-struct      :documentation "pkt-struct del pacchetto correntemente in invio.")
   (mac-seqnum      :initarg :mac-seqnum      :accessor mac-seqnum      :documentation "Numero di sequenza MAC per individuare wifi-frame duplicate")
   (mac-err-no      :initarg :mac-err-no      :accessor mac-err-no      :documentation "Numero attuale di errori di invio a livello MAC")
   (max-mac-err-no  :initarg :mac-max-err-no  :accessor max-mac-err-no  :documentation "Massimo numero di errori d'invio a livello MAC")
   (mac-retry-tmout :initarg :retry-tmout     :accessor retry-tmout     :documentation "Tempo di attesa prima di riprovare a inviare il frame")
   (mac-retry-event :initarg :retry-event     :accessor retry-event     :documentation "Riferimento all'evento schedulato per re-invio frame")
   (auto-nack-tmout :initarg :auto-nack-tmout :accessor auto-nack-tmout :documentation "Tempo di attesa entro cui se non viene notificato nulla si assume NACK")
   (auto-nack-event :initarg :auto-nack-event :accessor auto-nack-event :documentation "Riferimento all'evento per la notifica NACK automatica")
   ;; TODO ragionare su come e quando schedulare l'invio dei PING
   ;; Potrebbe essere `best-wlan' che schedula l'invio dei ping alle
   ;; interfacce che non sono state scelte come migliori.
   (ping-seqnum     :initarg :ping-seqnum     :accessor ping-seqnum     :documentation "Numero di sequenza del ping")
   (ping-send-tmout :initarg :ping-send-tmout :accessor ping-send-tmout :documentation "Tempo di intervallo tra l'invio di un ping e l'altro")
   (ping-send-event :initarg :ping-send-event :accessor ping-send-event :documentation "Evento di invio del ping")
   (pkt-log         :initarg :pkt-log         :accessor pkt-log         :documentation "Log dell'interfaccia, lista di pkt-log-entry.")))


(defmethod setup-new! ((wob ulb-wlan-out-bag))
  (set-unbound-slots wob
    (fw (random-pick (list (list :ack)
			   (list :nack)
			   (list :ack :nack))))
    (fw-guess (list))
    (pkt-struct nil)
    (mac-seqnum -1)
    (mac-err-no 0)
    (max-mac-err-no 7)             ; valore preso dal paper di ghini
    (mac-retry-tmout (usecs 3))    ; http://www.air-stream.org.au/ACK_Timeouts dice 2, ma il retry-event verrebbe schedulato prima
    (mac-retry-event nil)
    (auto-nack-tmout (usecs 20))
    (auto-nack-event nil)
    (ping-send-tmout nil)
    (ping-send-event nil))
  ;; TODO chiamare `clean!'
  (call-next-method))


(defclass ulb-wlan-in-fbag (fbag)
  ;; Riferimento alla wlan-out appaiata.
  ;; Serve perche' quando wlan-in riceve un ack, deve far smettere di inviare a wlan-out.
  ;; Inoltre deve accedere al log di wlan-out quando riceve ping e notifiche.
  ((sibling-wlan :initarg :sibling-wlan :accessor sibling-wlan :type ulb-wlan-out-bag)))

(defmethod setup-new! ((wib ulb-wlan-in-fbag))
  (set-unbound-slots wib
    (sibling-wlan (error "ulb-wlan-in-fbag: sibling-wlan necessaria!")))
  (call-next-method))



(defclass ulb-sim (sim)
  ((sendmsg-id :initarg :sendmsg-id :accessor sendmsg-id)
   (out        :initarg :out        :accessor out    :type ulb-out-fbag)
   (in         :initarg :in         :accessor in     :type in-fbag)
   (sent       :initarg :sent       :accessor sent   :type hash-table)
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


;; ACCESS POINT

(defclass ap-sim (sim)
  ((fromwifi   :initarg fromwifi    :accessor fromwifi :type a2b-fbag)
   (towifi     :initarg towifi      :accessor towifi   :type b2a-fbag)))

(defmethod setup-new! ((as ap-sim))
  (set-unbound-slots as
    (fromwifi (new 'a2b-fbag :owner as))
    (towifi   (new 'b2a-fbag :owner as)))
  (call-next-method))


;; PROXY

(defclass proxy-eth-in-fbag (in-fbag)
  nil)

(defclass proxy-eth-out-fbag (out-fbag)
  nil)

(defclass proxy-sim (sim)
  ((eth0-in   :initarg :eth0-in     :accessor eth0-in  :type proxy-eth-in-fbag)
   (eth0-out  :initarg :eth0-out    :accessor eth0-out :type proxy-eth-out-fbag)
   (eth1-in   :initarg :eth1-in     :accessor eth1-in  :type proxy-eth-in-fbag)
   (eth1-out  :initarg :eth1-out    :accessor eth1-out :type proxy-eth-out-fbag)
   (in        :initarg :in          :accessor in       :type in-fbag)
   (out       :initarg :out         :accessor out      :type out-fbag)))

(defclass proxy-stoca-sim (proxy-sim)
  nil)

(defmethod setup-new! ((ps proxy-sim))
  (set-unbound-slots ps
    (eth0-in  (new 'proxy-eth-in-fbag  :owner ps))
    (eth0-out (new 'proxy-eth-out-fbag :owner ps))
    (eth1-in  (new 'proxy-eth-in-fbag  :owner ps))
    (eth1-out (new 'proxy-eth-out-fbag :owner ps))
    (in       (new 'in-fbag            :owner ps))
    (out      (new 'out-fbag           :owner ps)))
  (with-slots (eth0-in eth0-out eth10-in eth1-out eth1-in in out) ps
    (connect! eth0-in out)
    (connect! eth1-in out)
    (connect! in eth0-out)
    (connect! in eth1-out))
  (call-next-method))


;; METODI ULB-SIM

(defmethod setup-new! ((us ulb-sim))
  (set-unbound-slots us
    (sendmsg-id -1)
    (out    (new 'ulb-out-fbag     :owner us))
    (in     (new 'in-fbag          :owner us))
    (sent   (make-hash-table))
    (w0-out (new 'ulb-wlan-out-bag :owner us))
    (w0-in  (new 'ulb-wlan-in-fbag :owner us :sibling-wlan (w0-out us)))
    (w1-out (new 'ulb-wlan-out-bag :owner us))
    (w1-in  (new 'ulb-wlan-in-fbag :owner us :sibling-wlan (w1-out us))))
  (with-slots (out in w0-out w0-in w1-out w1-in) us
    (connect! out w0-out)
    (connect! w0-in in)
    (connect! out w1-out)
    (connect! w1-in in))
  (call-next-method))


(defmethod sendmsg-getid! ((us ulb-sim))
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
  (let ((ps (new 'pkt-struct :pkt rp :tstamp (gettime!))))
    (in! us uob ps t t)))


(defmethod insert! ((uob ulb-out-fbag) (ps pkt-struct) &key)
  "Dentro a out le pkt-struct sono in ordine ascendente di timestamp"
  (call-next-method)
  (! (setf (elements uob)
	   (stable-sort (elements uob) #'< :key #'tstamp))))


(defmethod remove! ((uob ulb-out-fbag) &key)
  "Se un pkt-struct e' piu' vecchia di 150 ms viene scartata."
  (let ((ps (call-next-method)))
    (if (>= (- (gettime!) (tstamp ps))
	    (msecs 150))
	(remove! uob)   ; questo scartato, riprova
	ps)))


(defmethod out! ((us ulb-stoca-sim) (uob ulb-out-fbag)
		 dst-bag dst-sim)
  "out-fbag -> best wlan"
  (handler-bind ((access-temporarily-unavailable #'abort)
		 (access-denied #'abort)
		 (no-destination #'abort))
    ;; TODO scegliere best-wlan in modo piu' rigoroso.
    (let ((best-wlan (random-pick (dests uob))))
      (call-next-method us uob best-wlan (owner best-wlan)))))


;; ULB-WLAN-OUT-BAG


(defmethod clean? ((wob ulb-wlan-out-bag))
  (with-slots (elements pkt-struct mac-err-no mac-retry-event
			auto-nack-event) wob
    (and (null elements)
	 (null pkt-struct)
	 (zerop mac-err-no)
	 (or (null mac-retry-event)
	     (dead? mac-retry-event))
	 (or (null auto-nack-event)
	     (dead? auto-nack-event)))))


(defmethod clean! ((wob ulb-wlan-out-bag))
  (with-slots (elements pkt-struct mac-err-no mac-retry-event
			auto-nack-event) wob
    (setf elements nil)
    (setf pkt-struct nil)
    (setf mac-err-no 0)
    (when (not (null mac-retry-event))
      (cancel! mac-retry-event))
    (when (not (null auto-nack-event))
      (cancel! auto-nack-event))))


(defmethod insert! ((wob ulb-wlan-out-bag) (ps pkt-struct) &key)
  ;; incapsulamento pkt-struct in wifi-frame, impostazione e
  ;; incremento mac-seqnum.
  (let ((wf (new 'wifi-frame :pld (new 'udp-pkt :pld (pkt ps))
		 :seq (incf (mac-seqnum wob)))))
    (insert! wob wf)))


(defmethod remove! ((wob ulb-wlan-out-bag) &key)
  (let ((qty (length (elements wob))))
    (if (not (= 1 qty))
	(error "remove! ulb-wlan-out-bag: ~a elementi invece che solo 1!" qty)
	(clone (first (elements wob))))))


(defmethod in! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag) (ps pkt-struct)
		dst-bag dst-sim)
  (when (not (clean? wob))
    (error "insert! wob ps: wob non e' clean"))
  ;; un pacchetto alla volta: lock!
  (lock! wob)
  ;; salva sendmsg-id in pkt-struct
  (setf (sendmsg-id ps)
	(sendmsg-getid! (owner wob)))
  ;; `auto-nack-event'
  (with-slots (fw fw-guess pkt-struct mac-seqnum auto-nack-tmout
		  auto-nack-event) wob
    ;; se `fw-guess' non contiene :nack, schedulo un `auto-nack-event'
    (when (not (find :nack fw-guess))
      (setf auto-nack-event (new 'event :tm (+ (gettime!)
					       auto-nack-tmout)
				 :desc (format nil "auto-nack! ~a" wob)
				 :owner-id (id wob)
				 :fn (lambda ()
				       (auto-nack! wob))))
      (schedule! auto-nack-event))
    ;; salvataggio a parte della pkt-struct
    (setf pkt-struct ps))
  ;; inserimento
  (call-next-method)
  ;; Salvo copia pkt-struct in `sent' hash-table
  (let ((ps-copy (clone ps)))
    (setf (gethash (sendmsg-id ps-copy) (sent us))
	  ps-copy))
  ;; schedula invio del pacchetto al link.
  (schedule! (new 'event :owner-id (id us)
		  :desc (format nil "out! ~a ~a ~a ~a" us wob t t)
		  :tm (gettime!)
		  :fn (lambda ()
			(out! us wob t t)))))


(defmethod out! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag)
		 dst-bag dst-sim)
  (with-slots (mac-retry-tmout mac-retry-event) wob
    ;; `mac-retry-event', riprova quando non arriva MAC-ACK.
    (setf mac-retry-event (new 'event :tm (+ (gettime!)
					     mac-retry-tmout)
			       :desc (format nil "mac-retry! ~a" wob)
			       :owner-id (id wob)
			       :fn (lambda ()
				     (mac-retry! wob))))
    (schedule! mac-retry-event))
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method)))


(defmethod give-up! ((wob ulb-wlan-out-bag))
  (when (find :nack (fw wob))
    (notify-nack! (owner wob) wob (sendmsg-id (pkt-struct wob)))))


(defmethod mac-retry! ((wob ulb-wlan-out-bag))
  (with-slots (mac-err-no max-mac-err-no) wob
    ;; un errore in piu'
    (incf mac-err-no)
    ;; se abbiamo ancora possibilita' riprova, altrimenti desiste.
    (if (< mac-err-no
	   max-mac-err-no)
	(out! (owner wob) wob t t)    ; riprova
	(give-up! wob))))


(defmethod auto-nack! ((wob ulb-wlan-out-bag))
  (notify-nack! (owner wob) wob (sendmsg-id (pkt-struct wob))))


(defmethod notify-nack! ((us ulb-sim) (wob ulb-wlan-out-bag) sendmsg-id)
  (multiple-value-bind (pkt pkt?)
      (gethash sendmsg-id (sent us))
    (when pkt?
      (schedule! (new 'event :tm (gettime!)
		      :owner-id (id us)
		      :desc (format nil "da sent a in! ~a" sendmsg-id)
		      :fn (lambda ()
			    (in! us (out us) pkt t t))))))
  (clean! wob))


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
