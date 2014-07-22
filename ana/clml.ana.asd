(asdf:defsystem :clml.ana.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.ana
                :pathname "src/"
                :serial t
                :depends-on (:cl-ana
                             :hjs
                             )
                :components (
                             (:file "package")
                             (:file "plotting.lisp")
                             ))
