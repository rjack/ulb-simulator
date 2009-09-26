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


;; TODO PING
;; out parte con 20 ping, numero di sequenza N/A
;; choose-best-wlan guarda calcola il minore tra i ping-seqnum delle wlan
;;    se quello minore e' piu' piccolo di 10 allora siamo ancora nel bootstrap
;;    la wlan col ping-seqnum minore e' la best

;; quando una wlan-out riceve un pacchetto, se ping-seqnum < 10 il
;; pacchetto DEVE essere un ping.

;; le wlan-out hanno il ping timeout nullo quando ping-seqnum < 10
;; dopo i primi 10 ping, hanno timeout nullo quando sono le best-wlan


(declaim (optimize debug safety (speed 0)))
;(declaim (optimize (debug 0) (safety 0) speed))


(defgeneric clean? (wob))
(defgeneric allow-next-pkt! (wob))
(defgeneric mac-retry! (wob))
(defgeneric mac-give-up! (wob))
(defgeneric auto-nack! (wob sendmsg-id))
(defgeneric sendmsg-getid! (us))
(defgeneric notify-nack! (us wob sendmsg-id))
(defgeneric notify-ack! (us wob sendmsg-id))
(defgeneric mac-confirm! (wob))
(defgeneric sent->out! (us sendmsg-id))
(defgeneric sent->discard! (us sendmsg-id))
(defgeneric inject! (bag obj &key tm))
(defgeneric cancel-auto-nack! (wob sendmsg-id))


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

(defmethod print-object ((p pkt) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a" (pld p))))

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


(defclass ping (dummy-data-pkt)
  ((seq     :initarg :seq    :accessor seq)
   (score   :initarg :score  :accessor score)))

(defmethod setup-new! ((p ping))
  (set-unbound-slots p
    (seq   "N/A")
    (score "N/A")
    (pld   (bytes 4)))    ;; supponiamo due short?
  (call-next-method))

(defmethod print-object ((p ping) stream)
  (print-unreadable-object (p stream :type t)
    (format stream ":seq ~a :score ~a ~a"
	    (seq p) (score p) (pld p))))


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


(defclass eth-frame (pkt)
  nil)

(defmethod setup-new! ((ef eth-frame))
  (set-unbound-slots ef
    (hdr-size (bytes 18)))
  (call-next-method))


(defclass wifi-frame (pkt)
  ((seq       :initarg :seq        :accessor seq)))

(defmethod setup-new! ((wf wifi-frame))
  (set-unbound-slots wf
    (hdr-size (bytes 32)))
  (call-next-method))

(defclass wifi-ack-frame (wifi-frame)
  nil)

(defmethod setup-new! ((waf wifi-ack-frame))
  (set-unbound-slots waf
    (pld (new 'dummy-data-pkt :pld (bytes 0))))
  (call-next-method))


(defmethod print-object ((wf wifi-frame) stream)
  (print-unreadable-object (wf stream :type t)
    (format stream ":seq ~a ~a" (seq wf) (pld wf))))

(defclass pkt-struct (obj)
  ((tstamp     :initarg :tstamp     :accessor tstamp)
   (delivery-no :initarg :delivery-no :accessor delivery-no)
   (sendmsg-id :initarg :sendmsg-id :accessor sendmsg-id)
   (pkt        :initarg :pkt        :accessor pkt :type pkt)))


(defmethod setup-new! ((ps pkt-struct))
  (set-unbound-slots ps
    (delivery-no 0)
    (sendmsg-id "N/A")
    (tstamp "N/A")
    (pkt (error "pkt-struct: pkt necessario!")))
  (call-next-method))

(defmethod clone ((ps pkt-struct))
  (let ((copy (call-next-method)))
    (setf (pkt copy)
	  (clone (pkt ps)))
    copy))

(defmethod print-object ((ps pkt-struct) stream)
  (print-unreadable-object (ps stream :type t)
    (format stream ":tstamp ~a :sendmsg-id ~a ~a" (tstamp ps)
	    (sendmsg-id ps) (pkt ps))))


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
   (mac-retry-tmout :initarg :retry-tmout     :accessor mac-retry-tmout :documentation "Tempo di attesa prima di riprovare a inviare il frame")
   (mac-retry-event :initarg :retry-event     :accessor mac-retry-event :documentation "Riferimento all'evento schedulato per re-invio frame")
   (auto-nack-tmout :initarg :auto-nack-tmout :accessor auto-nack-tmout :documentation "Tempo di attesa entro cui se non viene notificato nulla si assume NACK")
   (auto-nack-table :initarg :auto-nack-table :accessor auto-nack-table :documentation "Tabella hash sendmsg-id -> evento auto-nack.")
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
    (mac-retry-tmout (msecs 4))
    (mac-retry-event nil)
    (auto-nack-tmout (msecs 30))
    (auto-nack-table (make-hash-table))
    (ping-send-tmout nil)
    (ping-send-event nil))
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

(defclass fromwifi-fbag (fbag)
  ((mac-seq :initarg :mac-seq :accessor mac-seq :documentation "Ultimo numero di sequenza visto in una wifi-frame, per scartare duplicati")))

(defclass ap-sim (sim)
  ((fromwifi   :initarg fromwifi    :accessor fromwifi :type fromwifi-fbag)
   (towifi     :initarg towifi      :accessor towifi   :type b2a-fbag)))

(defmethod setup-new! ((as ap-sim))
  (set-unbound-slots as
    (fromwifi (new 'fromwifi-fbag :owner as :mac-seq -1))
    (towifi   (new 'b2a-fbag      :owner as)))
  (call-next-method))


;; PROXY

(defclass proxy-eth-out-fbag (out-fbag)
  nil)

(defclass proxy-eth-in-fbag (in-fbag)
  ((sibling-eth :initarg :sibling-eth :accessor sibling-eth :type proxy-eth-out-fbag)))


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


;; METODI ACCESS POINT

(defmethod in! ((as ap-sim) (fw fromwifi-fbag) (wf wifi-frame)
		dst-bag dst-sim)
  ;; A prescindere che sia duplicata o meno, deve ackare la wf
  ;; arrivata, altrimenti il peer continua a mandarla.
  (let ((waf (new 'wifi-ack-frame :seq (seq wf))))
    ;; inserisco wifi-ack-frame nella towifi
    (schedule! (new 'event :tm (gettime!)
		    :desc (str "in! ~a ~a ~a t t" as (towifi as) waf)
		    :fn (lambda ()
			  (in! as (towifi as) waf t t)))))
  ;; controllo frame duplicate
  (if (<= (seq wf)
	  (mac-seq fw))
      (my-log "discard-duplicated ~a ~a" as wf)
      (progn
	(incf (mac-seq fw))
	(assert (= (seq wf)
		   (mac-seq fw))
		nil "Buco nella numerazione mac-seq!")
	;; creo ethernet frame e la inserisco.
	(let ((ef (new 'eth-frame :pld (pld wf))))
	  (in! as fw ef dst-bag dst-sim)))))


;; METODI PROXY

;; TODO

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

(defun choose-best-wlan (wlans)
  (let ((best (find-if #'(lambda (w)
			   (not (lock? w))) wlans)))
    (if (null best)
	(first wlans)
	best)))


;; ULB-OUT-FBAG

(defmethod in! ((us ulb-stoca-sim) (uob ulb-out-fbag) (up udp-pkt)
		dst-bag dst-sim)
  "RTP arriva su UDP da softphone"
  (let ((ps (new 'pkt-struct
	      :pkt (the rtp-pkt (pld up))
	      :tstamp (gettime!))))
    (in! us uob ps t t)))


(defmethod in! ((us ulb-stoca-sim) (uob ulb-out-fbag) (p ping)
		dst-bag dst-sim)
  "Ping iniettati in fase di setup, per ping burst."
  (call-next-method))


(defmethod insert! ((uob ulb-out-fbag) (ps pkt-struct) &key)
  "Dentro a out le pkt-struct sono in ordine ascendente di timestamp"
  (call-next-method)
  (! (setf (elements uob)
	   (stable-sort (elements uob) #'< :key #'tstamp))))


(defmethod out! ((us ulb-stoca-sim) (uob ulb-out-fbag)
		 dst-bag dst-sim)
  "out-fbag -> best wlan"
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (let ((best-wlan (choose-best-wlan (dests uob))))
      (call-next-method us uob best-wlan (owner best-wlan)))))


;; ULB-WLAN-OUT-BAG


(defmethod clean? ((wob ulb-wlan-out-bag))
  (with-slots (elements pkt-struct mac-err-no mac-retry-event) wob
    (and (null elements)
	 (null pkt-struct)
	 (zerop mac-err-no)
	 (or (null mac-retry-event)
	     (dead? mac-retry-event)))))


(defmethod allow-next-pkt! ((wob ulb-wlan-out-bag))
  (! (with-slots (elements pkt-struct mac-err-no mac-retry-event) wob
       (setf elements nil)
       (setf pkt-struct nil)
       (setf mac-err-no 0)
       (when (not (null mac-retry-event))
	 (cancel! mac-retry-event))
       (unlock! wob))))


(defmethod insert! ((wob ulb-wlan-out-bag) (ps pkt-struct) &key)
  ;; incapsulamento pkt-struct in wifi-frame, impostazione e
  ;; incremento mac-seqnum.
  (let ((wf (new 'wifi-frame :pld (new 'udp-pkt :pld (pkt ps))
		 :seq (incf (mac-seqnum wob)))))
    (insert! wob wf)))


(defmethod remove! ((wob ulb-wlan-out-bag) &key)
  ;; TODO: se e' un PING non va clonato ma va proprio rimosso
  (let ((qty (length (elements wob))))
    (if (not (= 1 qty))
	(error "remove! ulb-wlan-out-bag: ~a elementi invece che solo 1!" qty)
	(clone (first (elements wob))))))


(defmethod in! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag) (ps pkt-struct)
		dst-bag dst-sim)
  (when (not (clean? wob))
    (error "insert! wob ps: wob non e' clean"))
  (if (>= (- (gettime!) (tstamp ps))
	  (msecs 120))
      (my-log "stale-pkt ~a ~a ~a" us wob ps)
      (progn
	;; un pacchetto alla volta: lock!
	(lock! wob)
	;; salva sendmsg-id in pkt-struct
	(setf (sendmsg-id ps)
	      (sendmsg-getid! (owner wob)))
	(incf (delivery-no ps))
	(with-slots (fw fw-guess pkt-struct mac-seqnum auto-nack-tmout
			auto-nack-table) wob
	  ;; se `fw-guess' non contiene :nack, schedulo un `auto-nack'
	  (when (not (find :nack fw-guess))
	    (let ((auto-nack-ev (new 'event
				  :tm (+ (gettime!)
					 auto-nack-tmout)
				  :desc (str "auto-nack! ~a ~a" wob (sendmsg-id ps))
				  :fn (lambda ()
					(auto-nack! wob (sendmsg-id ps))))))
	      (setf (gethash (sendmsg-id ps) auto-nack-table)
		    auto-nack-ev)
	      (schedule! auto-nack-ev)))
	  ;; salvataggio a parte della pkt-struct
	  (setf pkt-struct ps))
	;; inserimento
	(call-next-method)
	;; Salvo copia pkt-struct in `sent' hash-table
	(let ((ps-copy (clone ps)))
	  (setf (gethash (sendmsg-id ps-copy) (sent us))
		ps-copy))
	;; schedula invio del pacchetto al link.
	(schedule! (new 'event
			:desc (str "out! ~a ~a ~a ~a" us wob t t)
			:tm (gettime!)
			:fn (lambda ()
			      (out! us wob t t)))))))


(defmethod out! ((us ulb-stoca-sim) (wob ulb-wlan-out-bag)
		 dst-bag dst-sim)
  (with-slots (mac-retry-tmout mac-retry-event) wob
    ;; `mac-retry-event', riprova quando non arriva MAC-ACK.
    (setf mac-retry-event (new 'event :tm (+ (gettime!)
					     mac-retry-tmout)
			       :desc (str "mac-retry! ~a" wob)
			       :fn (lambda ()
				     (mac-retry! wob))))
    (schedule! mac-retry-event))
  (handler-bind ((access-temporarily-unavailable #'wait)
		 (access-denied #'abort)
		 (no-destination #'abort))
    (call-next-method)))


(defmethod mac-confirm! ((wob ulb-wlan-out-bag))
  "La wlan-in associata ha ricevuto un mac-ack"
  (! (cancel! (mac-retry-event wob))
     (when (find :ack (fw wob))
       (notify-ack! (owner wob) wob (sendmsg-id (pkt-struct wob))))
     (allow-next-pkt! wob)))


(defmethod mac-retry! ((wob ulb-wlan-out-bag))
  (with-slots (mac-err-no max-mac-err-no) wob
    ;; un errore in piu'
    (incf mac-err-no)
    ;; se abbiamo ancora possibilita' riprova, altrimenti desiste.
    (if (< mac-err-no
	   max-mac-err-no)
	(out! (owner wob) wob t t)    ; riprova
	(mac-give-up! wob))))


(defmethod mac-give-up! ((wob ulb-wlan-out-bag))
  (! (when (find :nack (fw wob))
       (notify-nack! (owner wob) wob (sendmsg-id (pkt-struct wob))))
     (allow-next-pkt! wob)))


(defmethod sent->out! ((us ulb-stoca-sim) (sendmsg-id number))
  (! (multiple-value-bind (pkt pkt?)
	 (gethash sendmsg-id (sent us))
       (when pkt?
	 (remhash sendmsg-id (sent us))
	 (if (= 1 (delivery-no pkt))
	     (schedule! (new 'event :tm (gettime!)
			     :desc (str "da sent a in! ~a" sendmsg-id)
			     :fn (lambda ()
				   (in! us (out us) pkt t t))))
	     (my-log "too-much-deliveries ~a" pkt))))))


(defmethod sent->discard! ((us ulb-stoca-sim) (sendmsg-id number))
  (! (remhash sendmsg-id (sent us))))


(defmethod auto-nack! ((wob ulb-wlan-out-bag) (sendmsg-id number))
  ;; rimuove se' stesso
  (! (remhash sendmsg-id (auto-nack-table wob))
     (sent->out! (owner wob) sendmsg-id)))


(defmethod cancel-auto-nack! ((wob ulb-wlan-out-bag) (sendmsg-id number))
  (! (multiple-value-bind (ev ev?)
	 (gethash sendmsg-id (auto-nack-table wob))
       (when ev?
	 (progn
	   (cancel! ev)
	   (remhash sendmsg-id (auto-nack-table wob)))))))


(defmethod notify-nack! ((us ulb-sim) (wob ulb-wlan-out-bag)
			 sendmsg-id)
  (!
    ;; TODO: log interfaccia ricevuto nack
    (pushnew :nack (fw-guess wob))
    (sent->out! us sendmsg-id)
    (cancel-auto-nack! wob sendmsg-id)))


(defmethod notify-ack! ((us ulb-sim) (wob ulb-wlan-out-bag)
			sendmsg-id)
  (!
    ;; TODO: log interfaccia ricevuto ack
    (pushnew :ack (fw-guess wob))
    (sent->discard! us sendmsg-id)
    (cancel-auto-nack! wob sendmsg-id)))



;; ULB-WLAN-IN-FBAG

(defmethod in! ((us ulb-stoca-sim) (wib ulb-wlan-in-fbag)
		(wf wifi-frame) dst-bag dst-sim)
  "Ulb wlan-in riceve frame da link wireless."
  ;; TODO: e' un PING o e' un RTP?
  ;; se ping iface-log-ping e discard
  ;; altrimenti iface-log-rtp, spacchettamento e insert
  (error 'not-implemented))


(defmethod in! ((us ulb-stoca-sim) (wib ulb-wlan-in-fbag)
		(waf wifi-ack-frame) dst-bag dst-sim)
  "Ulb wlan-in riceve ack-frame da link wireless."
  (let ((wob (sibling-wlan wib)))
    (if (= (seq waf)
	   (mac-seqnum wob))
	(mac-confirm! wob)
	(my-log "stale-mac-ack ~a ~a" wib waf))))


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


;; SETUP

(defun inject-pings! (us)
  (dotimes (i 20)
    (inject! (out us) (new 'ping))))


;; DEBUG

(defmethod inject! ((b bag) (p pkt) &key (tm (gettime!)))
  (schedule! (new 'event :tm tm
		  :desc (str "inject ~a ~a ~a" (owner b) b p)
		  :fn (lambda ()
			(in! (owner b) b p t t)))))
