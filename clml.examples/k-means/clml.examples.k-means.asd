;;;; clml.examples.k-means.asd
(format t "Please set *read-default-float-format* to double-float
    (setq *read-default-float-format* 'double-float)")

  (asdf:defsystem #:clml.examples.k-means
    :serial t
    :description "k-means example for CLML"
    :author "Mike Maul <mike.maul@gmail.com>"
    :license "MIT"
    :depends-on (#:clml.hjs #:clml.utility)
    :components ((:file "k-means")))

