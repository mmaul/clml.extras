(asdf:defsystem #:clml.extras.docs
  :serial t
  :description "Documentation generation system for CLML.EXTRAS"
  :author "Mike Maul"
  ;:email "maul.mike@gmail.com"
  :license "MIT"
  :depends-on (
               :clod
               :iterate
               :cl-ppcre
               :clml.docs
               )
  :pathname "src/"
  :components ((:file "package")
               (:file "docs")
               ))
