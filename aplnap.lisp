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
			       (new 'rtp-pkt :pld (new 'dummy-data-pkt :pld (bytes 700)))
			       t t))))))


(defun run! ()
  (handler-case (loop :do (fire!))
    (no-events ()
      (format t "arrivati: ~a~%"
	      (length (elements (in *b-sp*)))))))
