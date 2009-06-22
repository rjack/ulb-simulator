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
;;     wifi-frame-in-port  --> +--------------+ --> data-out-port
;;           LINK              | ACCESS-POINT |        ETH0
;;     wifi-frame-out-port <-- +--------------+ <-- data-in-port
;;
;; handle-input wifi-frame: output payload to ETH0
;;                          output wifi frame ack to LINK
;; handle-input data: output wifi-frame to LINK



(in-package :ulb-sim)


(defclass access-point (simulator)
  ((to-link-wifi-frames
    :initform (list)
    :accessor to-link-wifi-frames-of
    :type list)
   (to-eth-data
    :initform (list)
    :accessor to-eth-data-of
    :type list)
   (wifi-frame-in
    :accessor wifi-frame-in-of
    :type wifi-frame-in-port)
   (wifi-frame-out
    :accessor wifi-frame-out-of
    :type wifi-frame-out-port)
   (data-in
    :accessor data-in-of
    :type data-in-port)
   (data-out
    :accessor data-out-of
    :type data-out-port)))


(defmethod port-ready ((ap access-point)
		       (wifi-in wifi-frame-in-port))
  (send-to-link ap))

(defmethod port-ready ((ap access-point)
		       (wifi-out wifi-frame-out-port))
  (send-to-link ap))

(defmethod port-ready ((ap access-point)
		       (data-in data-in-port))
  (send-to-eth ap))

(defmethod port-ready ((ap access-point)
		       (data-out data-out-port))
  (send-to-eth ap))


(defmethod handle-input ((ap access-point)
			 (wifi-frame-in wifi-frame-in-port)
			 (wf wifi-frame))
  (call-next-method)
  (with-accessors ((to-eth-data to-eth-data-of)
		   (to-link-wifi-frames to-link-wifi-frames-of)) ap
    (let ((must-send-eth-p (null to-eth-data))
	  (must-send-link-p (null to-link-wifi-frames)))
      (setf to-eth-data
	    (append to-eth-data
		    (list (wifi-frame->data wf))))
      (setf to-link-wifi-frames
	    (append to-link-wifi-frames
		    (list (fresh-wifi-ack-frame ap))))
      (nconc (when must-send-eth-p
	       (send-to-eth ap))
	     (when must-send-link-p
	       (send-to-link ap))))))


(defmethod handle-input ((ap access-point) (data-in data-in-port)
			 (da data))
  (call-next-method)
  (with-accessors ((to-link-wifi-frames to-link-wifi-frames-of)) ap
    (let ((must-send-p (null to-link-wifi-frames)))
      (setf to-link-wifi-frames
	    (append to-link-wifi-frames
		    (list (data->wifi-frame da))))
      (when must-send-p
	(send-to-link ap)))))


(defmethod output ((ap access-point) (out out-port) (obj object))
  (handler-bind ((port-not-connected #'abort)
		 (out-port-busy #'wait)
		 (in-port-busy #'wait))
    (call-next-method)))


(defmethod send-to-link ((ap access-point))
  (with-accessors ((to-link-wifi-frames to-link-wifi-frames-of)
		   (clock clock-of)
		   (wifi-frame-out wifi-frame-out-of)) ap
    (assert to-link-wifi-frames nil
	    "send-to-link has nothing to send!")
    (list (make-instance 'event
			 :time clock
			 :owner ap
			 :fn #'output
			 :args (list wifi-frame-out
				     (first to-link-wifi-frames))))))


(defmethod send-to-eth ((ap access-point))
  (with-accessors ((to-eth-data to-eth-data-of)
		   (clock clock-of)
		   (data-out data-out-of)) ap
    (assert to-eth-data nil
	    "send-to-eth has nothing to send!")
    (list (make-instance 'event
			 :time clock
			 :owner ap
			 :fn #'output
			 :args (list data-out
				     (first to-eth-data))))))


(defmethod remove-child ((ap access-point) (wf wifi-frame))
  (with-accessors ((to-link-wifi-frames to-link-wifi-frames-of)) ap
    (if (eq wf (first to-link-wifi-frames))
	(pop to-link-wifi-frames)
	(assert (null (find wf to-link-wifi-frames)) nil
		"Removing the wrong child"))
    (call-next-method)))

(defmethod remove-child ((ap access-point) (da data))
  (with-accessors ((to-eth-data to-eth-data-of)) ap
    (if (eq da (first to-eth-data))
	(pop to-eth-data)
	(assert (null (find da to-eth-data)) nil
		"Removing the wrong child"))
    (call-next-method)))
