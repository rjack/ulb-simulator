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


(defclass ulb-sim (scenario)
  ((alice
    :initarg :alice
    :initform (error ":alice missing")
    :accessor alice-of
    :type person)
   (a-softphone
    :initarg :a-softphone
    :initform (error ":a-softphone missing")
    :accessor a-softphone-of
    :type softphone)
   (a-ulb
    :initarg :a-ulb
    :initform (error ":a-ulb missing")
    :accessor a-ulb-of
    :type wifi-interface)
   (a-wlan0
    :initarg :a-wlan0
    :initform (error ":a-wlan0 missing")
    :accessor a-wlan0-of
    :type wifi-interface)
   (a-wlan1
    :initarg :a-wlan1
    :initform (error ":a-wlan1 missing")
    :accessor a-wlan1-of
    :type wifi-interface)
   (a-wifi0
    :initarg :a-wifi0
    :initform (error ":a-wifi0 missing")
    :accessor a-wifi0-of
    :type network-link)
   (a-wifi1
    :initarg :a-wifi1
    :initform (error ":a-wifi1 missing")
    :accessor a-wifi1-of
    :type network-link)
   (a-ap0-wlan
    :initarg :a-ap0-wlan
    :initform (error ":a-ap0-wlan missing")
    :accessor a-ap0-wlan-of
    :type wifi-interface)
   (a-ap1-wlan
    :initarg :a-ap1-wlan
    :initform (error ":a-ap1-wlan missing")
    :accessor a-ap1-wlan-of
    :type wifi-interface)
   (a-ap0
    :initarg :a-ap0
    :initform (error ":a-ap0 missing")
    :accessor a-ap0-of
    :type access-point)
   (a-ap1
    :initarg :a-ap1
    :initform (error ":a-ap1 missing")
    :accessor a-ap1-of
    :type access-point)
   (a-internet
    :initarg :a-internet
    :initform (error ":a-internet missing")
    :accessor a-internet-of
    :type network-link)
   (a-proxy
    :initarg :a-proxy
    :initform (error ":a-proxy missing")
    :accessor a-proxy-of
    :type proxy-server)))
