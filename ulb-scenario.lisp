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
;; TODO


(in-package :ulb-sim)


(defclass ulb-scenario (scenario)
  ((alice
    :initarg :alice
    :initform (error ":alice missing")
    :accesor alice-of
    :type person)
   (bob
    :initarg :bob
    :initform (error ":bob missing")
    :accessor bob-of
    :type person)
   (a-softphone
    :initarg :a-softphone
    :initform (error ":a-softphone missing")
    :accessor a-softphone-of
    :type softphone)
   (b-softphone
    :initarg :b-softphone
    :initform (error ":b-softphone missing")
    :accessor b-softphone-of
    :type softphone)
   (a-loopback-sp
    :initarg :a-loopback-sp
    :initform (error ":a-loopback-sp missing")
    :accessor a-loopback-sp
    :type network-interface)
   (b-loopback-sp
    :initarg :b-loopback-sp
    :initform (error ":b-loopback-sp missing")
    :accessor b-loopback-sp
    :type network-interface)
   (a-loopback-link
    :initarg :a-loopback-link
    :initform (error ":a-loopback-link missing")
    :accessor a-loopback-link
    :type network-link)
   (b-loopback-link
    :initarg :b-loopback-link
    :initform (error ":b-loopback-link missing")
    :accessor b-loopback-link
    :type network-link)
   (a-loopback-ulb
    :initarg :a-loopback-ulb
    :initform (error ":a-loopback-ulb missing")
    :accessor a-loopback-ulb
    :type network-interface)
   (b-loopback-ulb
    :initarg :b-loopback-ulb
    :initform (error ":b-loopback-ulb missing")
    :accessor b-loopback-ulb
    :type network-interface)
   (a-ulb
    :initarg :a-ulb
    :initform (error ":a-ulb missing")
    :accessor a-ulb
    :type network-interface)
   (b-ulb
    :initarg :b-ulb
    :initform (error ":b-ulb missing")
    :accessor b-ulb
    :type network-interface)
   (a-wlan0
    :initarg :a-wlan0
    :initform (error ":a-wlan0 missing")
    :accessor a-wlan-of
    :type network-interface)
   (b-wlan0
    :initarg :b-wlan0
    :initform (error ":b-wlan0 missing")
    :accessor b-wlan-of
    :type network-interface)
   (a-wifi-00
    :initarg :a-wifi-00
    :initform (error ":a-wifi-00 missing")
    :accessor a-wifi-00-of
    :type network-link)
   (a-wifi-01
    :initarg :a-wifi-00
    :initform (error ":a-wifi-01 missing")
    :accessor a-wifi-01-of
    :type network-link)
   (a-wifi-10
    :initarg :a-wifi-10
    :initform (error ":a-wifi-10 missing")
    :accessor a-wifi-10-of
    :type network-link)
   (a-wifi-11
    :initarg :a-wifi-11
    :initform (error ":a-wifi-11 missing")
    :accessor a-wifi-11-of
    :type network-link)
   (b-wifi-00
    :initarg :b-wifi-00
    :initform (error ":b-wifi-00 missing")
    :accessor b-wifi-00-of
    :type network-link)
   (b-wifi-01
    :initarg :b-wifi-00
    :initform (error ":b-wifi-01 missing")
    :accessor b-wifi-01-of
    :type network-link)
   (b-wifi-10
    :initarg :b-wifi-10
    :initform (error ":b-wifi-10 missing")
    :accessor b-wifi-10-of
    :type network-link)
   (b-wifi-11
    :initarg :b-wifi-11
    :initform (error ":b-wifi-11 missing")
    :accessor b-wifi-11-of
    :type network-link)
   ... TODO