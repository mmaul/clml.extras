(asdf:defsystem :clml.cl-plplot.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.cl-plplot
                :pathname "src/"
                :serial t
                :depends-on (
                             :cl-plplot
                             :clml.hjs
                             :clml.cl-plplot.package
                             )
                :components (
                             (:file "package")
                             (:file "plots")
                             ))
