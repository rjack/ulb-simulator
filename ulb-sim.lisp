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
   (a-wifi-0
    :initarg :a-wifi-0
    :initform (error ":a-wifi-0 missing")
    :accessor a-wifi-0-of
    :type network-link)
   (a-wifi-1
    :initarg :a-wifi-1
    :initform (error ":a-wifi-1 missing")
    :accessor a-wifi-1-of
    :type network-link)
   (b-wifi-0
    :initarg :b-wifi-0
    :initform (error ":b-wifi-0 missing")
    :accessor b-wifi-0-of
    :type network-link)
   (b-wifi-1
    :initarg :b-wifi-1
    :initform (error ":b-wifi-1 missing")
    :accessor b-wifi-1-of
    :type network-link)
   (a-ap0-wlan
    :initarg :a-ap0-wlan
    :initform (error ":a-ap0-wlan missing")
    :accessor a-ap0-wlan-of
    :type network-interface)
   (a-ap1-wlan
    :initarg :a-ap1-wlan
    :initform (error ":a-ap1-wlan missing")
    :accessor a-ap1-wlan-of
    :type network-interface)
   (b-ap0-wlan
    :initarg :b-ap0-wlan
    :initform (error ":b-ap0-wlan missing")
    :accessor b-ap0-wlan-of
    :type network-interface)
   (b-ap1-wlan
    :initarg :b-ap1-wlan
    :initform (error ":b-ap1-wlan missing")
    :accessor b-ap1-wlan-of
    :type network-interface)
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
   (b-ap0
    :initarg :b-ap0
    :initform (error ":b-ap0 missing")
    :accessor b-ap0-of
    :type access-point)
   (b-ap1
    :initarg :b-ap1
    :initform (error ":b-ap1 missing")
    :accessor b-ap1-of
    :type access-point)
   (a-ap0-eth
    :initarg :a-ap0-eth
    :initform (error ":a-ap0-eth missing")
    :accessor a-ap0-eth-of
    :type network-interface)
   (a-ap1-eth
    :initarg :a-ap1-eth
    :initform (error ":a-ap1-eth missing")
    :accessor a-ap1-eth-of
    :type network-interface)
   (b-ap0-eth
    :initarg :b-ap0-eth
    :initform (error ":b-ap0-eth missing")
    :accessor b-ap0-eth-of
    :type network-interface)
   (b-ap1-eth
    :initarg :b-ap1-eth
    :initform (error ":b-ap1-eth missing")
    :accessor b-ap1-eth-of
    :type network-interface)
   (a-internet
    :initarg :a-internet
    :initform (error ":a-internet missing")
    :accessor a-internet-of
    :type network-link)
   (b-internet
    :initarg :b-internet
    :initform (error ":b-internet missing")
    :accessor b-internet-of
    :type network-link)
   (a-proxy-eth0
    :initarg :a-proxy-eth0
    :initform (error ":a-proxy-eth0 missing")
    :accessor a-proxy-eth0-of
    :type network-interface)
   (b-proxy-eth0
    :initarg :b-proxy-eth0
    :initform (error ":b-proxy-eth0 missing")
    :accessor b-proxy-eth0-of
    :type network-interface)
   (a-proxy
    :initarg :a-proxy
    :initform (error ":a-proxy missing")
    :accessor a-proxy-of
    :type proxy-server)
   (b-proxy
    :initarg :b-proxy
    :initform (error ":b-proxy missing")
    :accessor b-proxy-of
    :type proxy-server)
   (a-proxy-eth1
    :initarg :a-proxy-eth1
    :initform (error ":a-proxy-eth1 missing")
    :accessor a-proxy-eth1-of
    :type network-interface)
   (b-proxy-eth1
    :initarg :b-proxy-eth1
    :initform (error ":b-proxy-eth1 missing")
    :accessor b-proxy-eth1-of
    :type network-interface)
   (ab-internet
    :initarg :ab-internet
    :initform (error ":ab-internet missing")
    :accessor ab-internet-of
    :type network-link)))
