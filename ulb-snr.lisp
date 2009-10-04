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
(defparameter *wire0* nil)
(defparameter *wire1* nil)
(defparameter *wireb* nil)
(defparameter *proxy* nil)
(defparameter *b-sp*  nil)


(defun set-trace ()
  (trace in! out! fire! schedule! wait access? mac-give-up! mac-confirm!
	 mac-retry! auto-nack! notify-ack! notify-nack! allow-next-pkt!
	 sendmsg-getid! cancel-auto-nack! sent->out! sent->discard!
	 choose-best-wlan))

(defun init! ()
  (setf *clock* 0)
  (setf *evs* (list))

  (setf *a-sp*  (new 'sphone-sim      :name "ALICE PHONE"))

  (setf *lo*    (new 'ln<->           :name "LO" :delay (msecs 1)))

  (setf *ulb*   (new 'ulb-stoca-sim   :name "ULB-STOCA"))

  (setf *wifi0* (new 'ln<->           :name "WIFI 0" :err-rate 0 :delay (msecs 1) :bw (megabits-per-second 10)))
  (setf *wifi1* (new 'ln<->           :name "WIFI 1" :err-rate 0 :delay (msecs 1) :bw (megabits-per-second 10)))

  (setf *ap0*   (new 'ap-sim          :name "AP 0"))
  (setf *ap1*   (new 'ap-sim          :name "AP 1"))

  (setf *wire0* (new 'ln<->           :name "WIRE 0" :delay (msecs 50) :bw (megabits-per-second 50)))
  (setf *wire1* (new 'ln<->           :name "WIRE 1" :delay (msecs 50) :bw (megabits-per-second 50)))

  (setf *proxy* (new 'proxy-stoca-sim :name "PROXY-STOCA"))

  (setf *wireb* (new 'ln<->           :name "WIRE B" :delay (msecs 20) :bw (kilobits-per-second 640)))

  (setf *b-sp*  (new 'sphone-sim      :name "BOB PHONE"))

  ;; a-sp / lo
  (connect! (out *a-sp*)   (a2b *lo*))
  (connect! (b2a *lo*)     (in *a-sp*))

  ;; lo / ulb
  (connect! (a2b *lo*)     (out *ulb*))
  (connect! (in *ulb*)     (b2a *lo*))

  ;; ulb / wifi0
  (connect! (w0-out *ulb*) (a2b *wifi0*))
  (connect! (b2a *wifi0*)  (w0-in *ulb*))

  ;; ulb / wifi1
  (connect! (w1-out *ulb*) (a2b *wifi1*))
  (connect! (b2a *wifi1*)  (w1-in *ulb*))

  ;; wifi0 / ap0
  (connect! (a2b *wifi0*)  (fromwifi *ap0*))
  (connect! (towifi *ap0*) (b2a *wifi0*))

  ;; wifi1 / ap1
  (connect! (a2b *wifi1*)  (fromwifi *ap1*))
  (connect! (towifi *ap1*) (b2a *wifi1*))

  ;; ap0 / wire0
  (connect! (fromwifi *ap0*) (a2b *wire0*))
  (connect! (b2a *wire0*) (towifi *ap0*))

  ;; ap1 / wire1
  (connect! (fromwifi *ap1*) (a2b *wire1*))
  (connect! (b2a *wire1*) (towifi *ap1*))

  ;; wire0 / proxy
  (connect! (a2b *wire0*) (eth0-in *proxy*))
  (connect! (eth0-out *proxy*) (b2a *wire0*))

  ;; wire1 / proxy
  (connect! (a2b *wire1*) (eth1-in *proxy*))
  (connect! (eth1-out *proxy*) (b2a *wire1*))

  ;; proxy / wireb
  (connect! (out *proxy*) (a2b *wireb*))
  (connect! (b2a *wireb*) (in *proxy*))

  ;; wireb / sp-b
  (connect! (a2b *wireb*) (in *b-sp*))
  (connect! (out *b-sp*) (b2a *wireb*)))


(defun run! ()
  (handler-bind ((access-temporarily-unavailable #'wait))
    (handler-case (loop :do (fire!))
      (no-events ()
	(my-log "Fine")))))


(defun run-pings+conversation ()
  (inject-pings! *ulb*)
  (run!)
  (schedule-conversation (out *a-sp*) (gettime!) 10)
  ;; TODO schedule errore
  (run!))
