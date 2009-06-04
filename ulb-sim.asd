(defpackage :ulb-sim.asd
  (:use :cl :asdf))

(in-package :ulb-sim.asd)


(defsystem ulb-sim
    :name "ulb-sim"
    :author "Giacomo Ritucci"
    :version "alpha"
    :license "2 clauses BSD style, see COPYING file for details"
    :depends-on ("de-sim")
    :components ((:file "ulb-sim.package")))
