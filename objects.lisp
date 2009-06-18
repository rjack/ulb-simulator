(declaim (optimize debug safety (speed 0)))
;(declaim (optimize (debug 0) (safety 0) speed))


(in-package :ulb-sim)



(defclass address ()
  ((name
    :initarg :name
    :initform (error ":name missing")
    :accessor name-of
    :type string)
   (port
    :initarg :port
    :initform (error ":port missing")
    :accessor port-of
    :type string)))


(defclass udp-packet (object)
  ((source
    :initarg :source
    :reader source-of
    :type address)
   (destination
    :initarg :destination
    :reader destination-of
    :type address)
   (payload
    :initarg :payload
    :accessor payload-of)
   (size
    :initarg :size
    :accessor size-of
    :type fixnum)))


(defclass voice (object)
  ((duration
    :initarg :duration
    :initform (error ":duration missing")
    :reader duration-of
    :type fixnum)))


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
