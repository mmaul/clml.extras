;;;; clml.asd
;(in-package "CL-USER")
#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
#+sbcl (declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
)


(let ((*read-default-float-format* 'double-float))
  (asdf:defsystem #:clml.extras
    :serial t
    :description "Enhancments to the base CLML (Common Lisp Machine Learming)"
    :author"
          Mike Maul  <maul.mike@gmail.com>"
    :maintainer "Mike Maul  <maul.mike@gmail.com>"
    :license "MIT"
    :depends-on (
                 :clml.ana
                 :clml.r-datasets
                 )
    ))


