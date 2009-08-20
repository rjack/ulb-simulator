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


(defpackage :it.unibo.cs.web.ritucci.ulb-snr
  (:nicknames :ulb-snm)
  (:use :common-lisp :de-sim :ulb-sim)
  (:export))
(in-package :ulb-snr)


;; ULB SCENARIO
;; Connette vari simulatori per costruire una situazione.



(defclass scenario ()
  ;; classe base con gli slot comuni a tutti gli scenari.
  ;; `monitor' e' una classe di de-sim.
  ((mon :initarg :mon :type monitor)))


(defclass ulb-scenario (scenario)
  ((sp    :initarg :sp    :type sphone-sim)
   (ulb   :initarg :ulb   :type ulb-sim)
   (wlan0 :initarg :wlan0 :type wlan-sim)
   (wlan1 :initarg :wlan1 :type wlan-sim)
   (ap0   :initarg :ap0   :type apoint-sim)
   (ap1   :initarg :ap1   :type apoint-sim)
   (proxy :initarg :proxy :type proxy-sim)))


(let* ((qos (pick-one 'none 'simple 'probabilistic))
       (talk (pick-one 'wild-talk 'balanced 'monosillabic))
       (snr (new 'ulb-scenario
		 :sp (new 'sphone-sim :talk talk)
		 :ulb (new 'ulb-sim :qos qos)
		 :wlan0 (new 'wlan-sim)
		 :wlan1 (new 'wlan-sim)
		 :ap0 (new 'apoint-sim)
		 :ap1 (new 'apoint-sim)
		 :proxy (new 'proxy-sim :qos qos))))
  nil)
