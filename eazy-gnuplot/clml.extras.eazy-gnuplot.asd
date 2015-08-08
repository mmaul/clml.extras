(asdf:defsystem #:clml.extras.eazy-gnuplot
  :description "Dataset plotting interface for CLML"
  :author "Mike Maul <mike.maul@gmail.com"
  :license "MIT"
  :serial t
  :depends-on (:clml.hjs
               :clml.time-series ; Need Time Series code
               :eazy-gnuplot
               )
  :components ((:file "package")
               (:file "plot")))
