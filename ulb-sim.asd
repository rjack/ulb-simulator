(defpackage :ulb-sim.asd
  (:use :cl :asdf))

(in-package :ulb-sim.asd)


(defsystem ulb-sim
    :name "ulb-sim"
    :author "Giacomo Ritucci"
    :version "alpha"
    :license "2 clauses BSD style, see COPYING file for details"
    :depends-on ("de-sim")
    :components ((:file "ulb-sim.package")
		 (:file "objects"
			:depends-on ("ulb-sim.package"))
		 (:file "ports"
			:depends-on ("ulb-sim.package" "objects"))
		 (:file "person"
			:depends-on ("ports" "objects" "ulb-sim.package"))
		 (:file "access-point"
			:depends-on ("ports" "objects" "ulb-sim.package"))
		 (:file "softphone"
			:depends-on ("ports" "objects" "ulb-sim.package"))
		 (:file "wifi-interface"
			:depends-on ("ports" "objects" "ulb-sim.package"))
		 (:file "network-links"
			:depends-on ("ports" "objects" "ulb-sim.package"))
		 (:file "proxy-server"
			:depends-on ("ports" "objects" "ulb-sim.package"))
		 (:file "ulb-sim"
			:depends-on ("ulb-sim.package"
				     "objects"
				     "ports"
				     "person"
				     "access-point"
				     "softphone"
				     "wifi-interface"
				     "network-links"
				     "proxy-server"))))
