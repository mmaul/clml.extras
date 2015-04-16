(asdf:defsystem :clml.r-datasets-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package"))
                )


(asdf:defsystem :clml.r-datasets
                :pathname "src/"
                :serial t
                :depends-on (
                             :cl-ppcre
                             :clml.hjs
                             :clml.utility
                             :clml.r-datasets-package
                             )
                :components (
                             (:file "r-datasets")
                             )
                )
