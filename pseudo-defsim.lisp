;; port types: simpex-out, simplex-in, half-duplex, full-duplex.

(defport audio simplex
  :carries sound)

(defport ethernet half-duplex
  :carries ethernet-frame)

(defport wireless half-duplex
  :carries wireless-frame)


(defsim access-point
  :properties
  ((:label name :type string :documentation "example: almawifi"))
  :ports
  ((:label wired :type ethernet :wait-if-locked t)
   (:label antenna :type wi-fi :wait-if-locked t)))


(defsim person
  :properties
  ((:label name :type string :documentation "example: Alice"))
  :out-ports
  ((:label mouth :type audio :if-locked wait))
  :in-ports
  ((:label ear :type audio)))


(defevents person ear voice
  ;; create and return an event that immediately discards the voice
  ;; object.
  (:immediately discard person ear voice))


(defevents person mouth voice
  ;; create and return an event that immediately outputs the first
  ;; object in mouth's queue.
  (:immediately next person mouth voice))