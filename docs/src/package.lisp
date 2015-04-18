(in-package :cl-user)

(defpackage :clml.extras.docs
  (:use
   :common-lisp
   :iterate
   :cl-ppcre
   :clod
   :clml.docs
   )
  (:shadow :generate-api-docs)
  (:export :generate-api-docs)
  (:documentation
   "API Documentation Generation System for CLML"
   )
  )
