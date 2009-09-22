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


(defparameter *a-sp*  nil)
(defparameter *lo*    nil)
(defparameter *ulb*   nil)
(defparameter *wifi0* nil)
(defparameter *wifi1* nil)
(defparameter *ap0*   nil)
(defparameter *ap1*   nil)


;(trace in! out! fire! remove! insert! peek dead? flush? schedule! wait access? wakeup!)


(defun init! (num)
  (setf *clock* 0)
  (setf *evs* (list))

  (setf *a-sp*  (new 'sphone-sim :name "ALICE PHONE"))

  (setf *lo*    (new 'ln<-> :name "LO"))

  (setf *ulb*   (new 'ulb :name "ULB"))

  (setf *wifi0* (new 'ln<-> :name "WIFI 0"))
  (setf *wifi1* (new 'ln<-> :name "WIFI 1"))

  (setf *ap0*   (new 'ap-sim :name "AP 0"))
  (setf *ap1*   (new 'ap-sim :name "AP 1"))

  (connect! (out *a-sp*)   (a2b *lo*))
  (connect! (b2a *lo*)     (in *a-sp*))

  (connect! (a2b *lo*)     (out *ulb*))
  (connect! (in *ulb*)     (b2a *lo*))

  (connect! (w0-out *ulb*) (a2b *wifi0*))
  (connect! (b2a *wifi0*)  (w0-in ulb))

  (connect! (w1-out *ulb*) (a2b *wifi1*))
  (connect! (b2a *wifi1*)  (w1-in ulb))

  (connect! (a2b *wifi0*)  (fromwifi *ap0*))
  (connect! (towifi *ap0*) (b2a *wifi0*))

  (connect! (a2b *wifi1*)  (fromwifi *ap1*))
  (connect! (towifi *ap1*) (b2a *wifi1*))

  (dotimes (i num)
    (schedule! (new 'event :tm (msecs i)
		    :desc "kickstart"
		    :owner-id (id *a-sp*)
		    :fn (lambda ()
			  (in! *a-sp* (out *a-sp*)
			       (new 'rtp-pkt)
			       t t))))))


(defun run! ()
  (handler-case (loop :do (fire!))
    (no-events ()
      (format t "arrivati: ~a~%"
	      (length (elements (in *b-sp*)))))))
