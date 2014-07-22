(in-packge clml.ana.plotting)

(in-package :plotting)
(defmethod line ((vector2d simple-vector) &rest line-kw-args)
  "clml data is mostly contained in vectors, make it easy to pass it directly"
  (apply #'line (cons  (mapcar (lambda (x) (coerce x 'list)) (coerce  vector2d 'list)) line-kw-args))
  )



