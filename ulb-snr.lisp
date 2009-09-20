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


(defparameter *a-sp* nil)
(defparameter *b-sp* nil)
(defparameter *rtp<->* nil)


(trace in! out! fire! remove! insert! peek dead? flush? schedule! wait access? wakeup!)


(defun init! (num)
  (setf *clock* 0)
  (setf *evs* (list))

  (setf *a-sp*   (new 'sphone-sim :name "ALICE PHONE"))
  (setf *b-sp*   (new 'sphone-sim :name "BOB PHONE"))
  (setf *rtp<->* (new 'ln<-> :name "RTP<->"
		      :bw (kilobytes-per-second 80)
		      :delay (msecs 100) :err-rate 0))

  (connect! (out *a-sp*) (a2b *rtp<->*))
  (connect! (a2b *rtp<->*) (in *b-sp*))

  (connect! (out *b-sp*) (b2a *rtp<->*))
  (connect! (b2a *rtp<->*) (in *a-sp*))


  (dotimes (i num)
    (schedule! (new 'event :tm (msecs i)
		    :desc "kickstart"
		    :owner-id (id *a-sp*)
		    :fn (lambda ()
			  (in! *a-sp* (out *a-sp*)
			       (new 'rtp-packet)
			       t t))))))


(defun run! ()
  (handler-case (loop :do (fire!))
    (no-events ()
      (format t "arrivati: ~a~%"
	      (length (elements (in *b-sp*)))))))
