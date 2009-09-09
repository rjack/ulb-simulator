;; Finta trace degli eventi dal primo evento del simulatore.

;; Tentativo: link manager globale, nella closure della funzione
;; links
;; e' un'hash table con gli id come key e valori una lista di
;; riferimenti a sim

(let ((link-map (make-hash-table)))

  (defun links (sim)
    (gethash (id sim) link-map))

  (defun set-links (sim &rest links)
    (setf (gethash (id sim) link-map)
	  links)))


;; KICKSTART: sphone di alice manda un pacchetto rtp

;; evento kickstart, definito nello scenario
(event :owner rif-asphone
       :tm random-time
       :fn (lambda (asphone)
	     (out asphone (outq asphone))))


(defmethod out ((sp sphone) (oq outq-el))
  (let ((up (new 'udp-packet :payload (pop oq))))
    ;; dando per scontato che oq non sia vuota
    (list (new 'event :owner sp :tm (tm sp)
	       :fn (lambda (sphone)
		     (handler-bind ((not-connected #'abort))
		       (access-link sphone oq up)))))))


(defmethod access-link ((sp sphone) (oq outq-el) (up udp-packet))
  (let ((ln (first (links oq))))
    (list (new 'event :owner ln :tm (tm sp)
	       :fn (lambda (ln)
		     (handler-bind ((access-denied #'wait))
		       (in ln sp up)))))))



;; come so quale dei due peer e'?
(defmethod in ((ln udp<->) (sender sim) (up udp-packet))
  (if (lock ln) ; in caso di full-duplex, come so quale lock controllare?
      (error 'access-denied)
      (enqueue (outq
      (list (new 'event :owner ln :tm calcolo-transfer-time
		 :fn (lambda (ln)
		       ;; TODO
		       nil)))))))