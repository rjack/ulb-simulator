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


;; ULB SCENARIO SIMULATOR
;; Istanzia e connette vari simulatori per costruire una situazione.


;; definizione dello scenario
;; E' un simulatore che ha come componenti i simulatori che compongono
;; lo scenario.
(defclass ulb-scenario (sim)
  ((sp-alice :initarg :sp-alice :type sphone-sim)
   (ulb      :initarg :ulb      :type ulb-sim)
   (wlan0    :initarg :wlan0    :type wlan-sim)
   (wlan1    :initarg :wlan1    :type wlan-sim)
   (ap0      :initarg :ap0      :type apoint-sim)
   (ap1      :initarg :ap1      :type apoint-sim)
   (proxy    :initarg :proxy    :type proxy-sim)
   (sp-bob   :initarg :sp-bob   :type sphone-sim)))


;; prova di creazione scenario
(let* ((qos (pick-one 'none 'simple 'probabilistic))
       (alice-talk (pick-one 'wild-talk 'balanced 'monosillabic))
       (bob-talk (pick-one 'wild-talk 'balanced 'monosillabic))
       ;; ogni tipo di link dovrebbe avere valori di default
       ;; appropriati.
       (links (list (udp<-> 'sp-alice 'ulb)
		    (udp<-> 'ulb 'wlan0)
		    (udp<-> 'ulb 'wlan1)
		    (netlink-> 'wlan0 'ulb)
		    (netlink-> 'wlan1 'ulb)
		    (wifi<-> 'wlan0 'ap0
			     :bw (mib/s 18)
			     :error-rate (% 10)
			     :delay (usecs 1))
		    (wifi<-> 'wlan1 'ap1
			     :bw (mib/s 13)
			     :error-rate (% 3)
			     :delay (usecs 1))
		    (udp<=> 'ap0 'proxy
			    :bw (mib/s 10)
			    :error-rate (% 2)
			    :delay (msecs 30))
		    (udp<=> 'ap1 'proxy
			    :bw (mib/s 12)
			    :error-rate (% 3)
			    :delay (msecs 17))
		    (udp<=> 'proxy 'sp-bob
			    :bw (mib/s 1)
			    :error-rate (% 1)
			    :delay (msecs 50))))
       (snr (new 'ulb-scenario
		 :sp-alice (new 'sphone-sim :talk alice-talk)
		 :ulb (new 'ulb-sim :qos qos)
		 :wlan0 (new 'wlan-sim)
		 :wlan1 (new 'wlan-sim)
		 :ap0 (new 'apoint-sim)
		 :ap1 (new 'apoint-sim)
		 :proxy (new 'proxy-sim :qos qos)
		 :sp-bob (new 'sphone-sim :talk (bob-talk))
		 :lm (new 'link-manager :links links))))
  nil)
