(declaim (optimize debug safety (speed 0)))
;(declaim (optimize (debug 0) (safety 0) speed))


(in-package :ulb-sim)


(defun usecs (us)
  us)

(defun msecs (ms)
  (* ms (usecs 1000)))

(defun secs (s)
  (* s (msecs 1000)))

(defun mins (m)
  (* m (secs 60)))


(defun bits (b)
  b)

(defun bytes (b)
  (* b (bits 8)))


(defun kilobits (kb)
  (* kb (bits (expt 10 3))))

(defun kilobytes (kb)
  (* kb (bytes (expt 10 3))))

(defun kibibits (kib)
  (* kib (bits (expt 2 10))))

(defun kibibytes (kib)
  (* kib (bytes (expt 2 10))))


(defun megabits (mb)
  (* mb (bits (expt 10 6))))

(defun megabytes (mb)
  (* mb (bytes (expt 10 6))))

(defun mebibits (mib)
  (* mib (bits (expt 2 20))))

(defun mebibytes (mib)
  (* mib (bytes (expt 2 20))))


(defun bits-per-second (bps)
  (/ (bits bps) (secs 1)))

(defun bytes-per-second (bps)
  (/ (bytes bps) (secs 1)))


(defun kilobits-per-second (kbps)
  (/ (kilobits kbps) (secs 1)))

(defun kilobytes-per-second (kbps)
  (/ (kilobytes kbps) (secs 1)))

(defun kibibits-per-second (kibps)
  (/ (kibibits kibps) (secs 1)))

(defun kibibytes-per-second (kibps)
  (/ (kibibytes kibps) (secs 1)))


(defun megabits-per-second (mbps)
  (/ (megabits mbps) (secs 1)))

(defun megabytes-per-second (mbps)
  (/ (megabytes mbps) (secs 1)))

(defun mebibits-per-second (mbps)
  (/ (mebibits mbps) (secs 1)))

(defun mebibytes-per-second (mbps)
  (/ (mebibytes mbps) (secs 1)))



;; Classes for various types of packets.

;; PACKET: base class
;; - overhead-size is the size of the headers and trailers of the
;;   packet. If the overhead informations themselves are needed in the
;;   simulation, they must be represented by additional slots on the
;;   subclass (see UDP-PACKET).
;; - payload can be nil or another incapsulated packet.

;; DATA: a chunk of bytes
;; no overhead, just data.
;; - payload-size is the size of the payload
;; - payload can be nil if the payload-size is the only information
;;   needed by the simulation or can point to any LISP object that
;;   represents the information.

;; RTP-PACKET: just size.
;; The RTP payload contains audio/video informations, not represented
;; by ulb-sim.

;; UDP-PACKET:
;; The UPD payload can carry RTP or RAW packets.
;; SOURCE and DESTINATION store the addresses written in the UDP
;; header.


(defun clone (obj)
  (when (not (null obj))
    (let ((copy (make-instance (type-of obj))))
      (transform copy obj)
      copy)))


(defclass packet (object)
  ((overhead-size
    :initform (bytes 0)
    :accessor overhead-size-of
    :type fixnum)
   (payload
    :initarg :payload
    :accessor payload-of)))


(defclass data (packet)
  ((payload
    :initform nil
    :accessor payload-of)
   (payload-size
    :initarg :payload-size
    :accessor payload-size-of
    :type (integer 0))))


(defclass rtp-packet (packet)
  ((overhead-size
    :initform (bytes 12))
   (payload
    :initform nil)))


(defclass udp-packet (packet)
  ((payload
    :type (or rtp-packet data))
   (overhead-size
    :initform (bytes 8))
   (source
    :initarg :source
    :accessor source-of)
   (destination
    :initarg :destination
    :accessor destination-of)))


(defclass wifi-frame (packet)
  ((overhead-size
    :initform (bytes 32))
   (payload
    :initform nil
    :type (or null udp-packet data))))


(defclass wifi-ack-frame (wifi-frame)
  ((payload
    :initform nil)))


(defmethod size ((pkt packet))
  (+ (overhead-size-of pkt)
     (size (payload-of pkt))))


(defmethod size ((pkt data))
  (payload-size-of pkt))


(defmethod size ((n null))
  0)


(defclass voice (object)
  ((duration
    :initarg :duration
    :accessor duration-of
    :type fixnum)))


(defmethod voice->rtp-packets ((vo voice)
			       &key codec-bw rtp-min rtp-max)
  (labels ((random-packet-size ()
	     (+ rtp-min
		(random (1+ (- rtp-max rtp-min)))))
	   (packets (nbytes-left list-acc)
	     (if (zerop nbytes-left)
		 list-acc
		 (let ((size (min nbytes-left
				  (random-packet-size))))
		   (packets (- nbytes-left size)
			    (cons (make-instance 'rtp-packet
						 :payload-size size)
				  list-acc))))))
    (let ((nbytes (* codec-bw (duration-of vo))))
      (nreverse (packets nbytes (list))))))


(defmethod rtp-packet->voice ((rp rtp-packet) &key codec-bw)
  (make-instance 'voice :duration (/ (size (payload-of rp))
				     codec-bw)))


(defmethod rtp-packet->udp-packet ((rp rtp-packet) &key src dst)
  (make-instance 'udp-packet
		 :source src
		 :destination dst
		 :payload rp))


(defmethod udp-packet->wifi-frame ((up udp-packet))
  (make-instance 'wifi-frame
		 :payload up))


(defmethod transform ((copy packet) (orig packet))
  (when (slot-boundp orig 'overhead-size)
    (setf (overhead-size-of copy) (overhead-size-of copy)))
  (when (slot-boundp orig 'payload)
    (setf (payload-of copy) (clone (payload-of orig)))))


(defmethod transform ((copy data) (orig data))
  (when (slot-boundp orig 'payload-size)
    (setf (payload-size-of copy) (payload-size-of orig)))
  (call-next-method))


(defmethod transform ((copy udp-packet) (orig udp-packet))
  (when (slot-boundp orig 'source)
    (setf (source-of copy) (source-of orig)))
  (when (slot-boundp orig 'destination)
    (setf (destination-of copy) (destination-of orig)))
  (call-next-method))
