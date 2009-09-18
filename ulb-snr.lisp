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


(defparameter *a-sp* (new 'sphone-sim :name "ALICE PHONE"))
(defparameter *b-sp* (new 'sphone-sim :name "BOB PHONE"))
(defparameter *rtp<->* (new 'ln<-> :name "RTP<->" :bw 5 :delay 10 :err-rate 50))

(connect! (out *a-sp*) (a2b *rtp<->*))
(connect! (a2b *rtp<->*) (in *b-sp*))

(connect! (out *b-sp*) (b2a *rtp<->*))
(connect! (b2a *rtp<->*) (in *a-sp*))


(trace in! out! fire! remove! insert! peek dead? flush? schedule!)


(dotimes (i 100)
  (schedule! (new 'event :tm i
		  :owner-id (id *a-sp*)
		  :fn (lambda ()
			(in! *a-sp* (out *a-sp*) (new 'rtp-packet))))))
