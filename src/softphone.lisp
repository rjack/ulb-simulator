;; ULB Simulator
;: UDP Load Balancer Simulator

;; Copyright (C) 2008  Giacomo Ritucci

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


(defpackage :it.unibo.cs.web.ritucci.ulb-sim.softphone
  (:nicknames :sp)
  (:use :common-lisp :ds))

(in-package :sp)


(let ((codec-bw (kbps 16))
      (rtp-payload-min-size 300)
      (rtp-payload-max-size 700))

  (defun voice->rtp-packets (duration)
    (let ((nbytes (* duration codec-bw)))
      (labels ((rand-packet-size ()
		 "Randomly choose the size of the next packet."
		 (random-between rtp-payload-min-size
				 rtp-payload-max-size))
	       (slice-and-cons (nbytes-left packet-size list-acc)
		 "Recursively cons the packet list."
		 (if (>= packet-size nbytes-left)
		     (cons (make-instance 'rtp-packet
					  :payload-size nbytes-left)
			   list-acc)
		     (slice-and-cons (- nbytes-left packet-size)
				     (rand-packet-size)
				     (cons (make-instance 'rtp-packet
							  :payload-size
							  packet-size)
					   list-acc)))))

	(slice-and-cons nbytes (rand-packet-size) nil)))))


(defclass softphone (simulated)
  "TODO"
  nil)
