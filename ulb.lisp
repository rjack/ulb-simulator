;; ULB-SIM
;; UDP Load Balancing SIMulator

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


;; OVERVIEW
;;
;;     udp-in-port  --> +-----+ --> udp-out-port
;;                      | ULB |       WLAN0
;;      LOOPBACK        |     | <--  udp-in-port
;;                      |     |
;;      INTERFACE       |     | --> udp-out-port
;;                      |     |       WLAN1
;;     udp-out-port <-- +-----+ <--  udp-in-port


(in-package :ulb-sim)


(defclass ulb-struct-datagram ()
  ((id
    :accessor id-of
    :documentation "assegnato da sendmsg-getid")
   (end-of-life-event
    :accessor end-of-life-event-of
    :documentation "Riferimento all'evento che rimuove questo pacchetto
     dall'ULB")
   (send-again-event
    :initarg :send-again-event
    :initform (error ":send-again-event mancante")
    :accessor send-again-event-of
    :documentation "Riferimento all'evento che pone questo pacchetto
     nuovamente nella coda di spedizione dall'ULB")
   (data
    :initarg :data
    :initform (error ":data mancante")
    :reader data-of
    :documentation "Riferimento al pacchetto vero e proprio.")))


(defclass ulb-struct-ping (ulb-struct-datagram)
  ;; Lo slot data punta a un istanza ping-packet che contiene numero di
  ;; sequenza e voto.
  nil)


(defclass first-hop-outcome ()
  ((dgram-id
    :initarg :dgram-id
    :initform (error ":dgram-id mancante")
    :reader dgram-id
    :documentation "ID del datagram, sarebbe assegnato da sendmsg-getid")
   (timestamp
    :initarg :timestamp
    :initform (error ":timestamp mancante")
    :reader timestamp
    :documentation "L'istante di creazione di questo first-hop-outcome")
   (value
    :initarg :value
    :initform (error ":value mancante")
    :reader value
    :documentation "Valore dell'outcome: ack oppure nak")))
;; TODO ack se TED dice ack oppure se ulb sa che ifaccia dice solo NAK ed
;; e' scaduto un timeout.
;; TODO nak se TED dice nak oppure se ulb sa che ifaccia dice solo ACK ed
;; e' scaduto un timeout.


(defclass full-path-outcome ()
  ((sequence-number
    :initarg :sequence-number
    :initform (error ":sequence-number mancante")
    :reader sequence-number
    :documentation "Numero di sequenza del ping a cui si riferisce questo
     full-path-outcome.")
   (ping-sent-at
    :initform *now*
    :reader ping-sent-at
    :documentation "Istante di spedizione del ping.")
   (ping-recv-at
    :accessor ping-recv-at
    :documentation "Istante di ricezione del ping di risposta.")))


(defclass ulb-wifi-interface ()
  ((firmware-detected
    :initform nil
    :accessor firmware-detected
    :documentation "ack, nak oppure full. Indica cio' che ULB ha dedotto
     del firmware della scheda, osservando le notifiche e i ping ricevuti.")
   (sent-datagrams
    :initform ()
    :accessor sent-datagrams
    :documentation "Gli ulb-struct-datagram (ping compresi) spediti da
     un'interfaccia vengono accodati qui in attesa di un ACK o di un
     end-of-life-event che li scarti, oppure di un NAK o di un
     send-again-event che li ritrasmetta.")
   (send-ping-event
    :initform nil
    :accessor send-ping-event
    :documentation "Riferimento all'evento che spedira' il prossimo ping su
     questa interfaccia.")

   (current-ping-seqnum
    :initform -1
    :accessor current-ping-seqnum
    :documentation "Numero di sequenza dell'ultimo ping spedito su questa
     interfaccia.")

   (score
    :initform 0
    :accessor score
    :documentation "Voto dell'interfaccia")

   (first-hop-log
    :initform nil
    :accessor first-hop-log
    :documentation "Lista di first-hop-outcome.")

   (full-path-log
    :initform nil
    :accessor full-path-log
    :documentation "Lista di full-path-outcome.")))


(defclass ulb (simulator)
  ((wlan-0
    :initarg :wlan-0
    :initform (error ":wlan-0 missing")
    :accessor wlan-0-of
    :type ulb-wifi-interface)
   (wlan-1
    :initarg :wlan-1
    :initform (error ":wlan-1 missing")
    :accessor wlan-1-of
    :type ulb-wifi-interface)
   (to-net-packets)
   (to-phone-packets)))



(defmethod handle-input ((u ulb) (lo-udp-in lo-udp-in-port)
			 (up udp-packet))
  "Incoming packet from softphone.
   Lifetime: 150ms"
  (call-next-method)
  (let ((eol-ev (fresh-eol-event)))
    (let ((dgram-struct (make-instance 'ulb-struct-datagram
				       :eol-ev eol-ev
				       :data (payload-of up)))
	  (must-send-p (null (to-net-packets-of u))))
      (setf (to-net-packets-of u)
	    (append (to-net-packets-of u)
		    (list dgram-struct)))
      (cons eol-ev (when must-send-p
		     (send-datagram (best-interface u)
				    (first-to-send u)))))))


(defmethod handle-input ((u ulb) (wlan-udp-in wlan-udp-in-port)
			 (pkt udp-packet))
  "Incoming packet from the net. Find the wlan interface owning this
   port and call recv-datagram."
  (let ((iface (find ...)))
    (assert iface nil "receiving interface not found!")
    (recv-datagram u iface pkt)))


(defmethod handle-input ((u ulb) (wlan-notify-in wlan-notify-in-port)
			 (ntf notification))
  "ack or nack?")


(defmethod recv-datagram ((u ulb) (uwi ulb-wifi-interface)
			  (pkt udp-packet))
  "Enqueue in to-phone")


(defmethod recv-datagram ((u ulb) (uwi ulb-wifi-interface)
			  (ping ping-packet))
  "Record in full path log")


(defmethod send-datagram ((u ulb) (uwi ulb-wifi-interface)
			  (dg ulb-struct-datagram))
  "reset ping timeout
   record log
   if not ping store datagram"
  (error "TODO"))
