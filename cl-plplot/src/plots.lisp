(in-package :clml.cl-plplot)



(defun prep-boxplot-data (&rest series-vectors)
  "Computes requsite parameters to construct a box plot for the supplied series vectors

-returns: <list> list containing
  +first-quartile
  +median
  +third-quartile
  +lower-whisker
  +upper-whisker
  +lower-outliers
  +lower-suspected-outliers
  +upper-suspected-outliers
  +upper-outliers
  +min
  +max
- arguments:
  -series-vectors: one or more series vectors of double-float."
  (make-array
   (list  (length series-vectors) 11) :initial-contents
   (mapcar (lambda (data)
             (multiple-value-bind (min max)
                    (loop for item across data
                       minimizing item into min
                       maximizing item into max
                       finally (return (values min max)))
               (let* ((first-quartile (discrete-quantile-on-sorted data 1/4))
                      (third-quartile (discrete-quantile-on-sorted data 3/4))
                      (median (median data))
                      (iqr (- third-quartile first-quartile))
                      (iqr*1.5 (* iqr 1.5))
                      (iqr*3 (* iqr 3))
                      (lower-inner-fence (if (< (- first-quartile (* iqr 1.5)) min) min
                                             (- first-quartile (* iqr 1.5))))
                      (upper-inner-fence (+ third-quartile (* iqr 1.5))))
                 #+sbcl (declare (ignorable iqr*3 iql*1.5))
                 (multiple-value-bind  (lower-outliers lower-suspected-outliers
                                                       upper-suspected-outliers upper-outliers)
                     (loop for item across data
                        when (and (> item third-quartile) (< item upper-inner-fence))
                        collect item into upper-suspected-outliers
                        when (> item upper-inner-fence)
                        collect item into upper-outliers
                        when (and (< item first-quartile) (> item lower-inner-fence))
                        collect item into lower-suspected-outliers
                        when (< item lower-inner-fence)
                        collect item into lower-outliers
                        finally (return (values lower-outliers lower-suspected-outliers
                                                upper-suspected-outliers upper-outliers))
                          )
                   (let*  ((lower-whisker
                            (if (or (> (length lower-outliers) 0)
                                    (> (length lower-suspected-outliers) 0))
                                lower-inner-fence
                                min))
                           (upper-whisker
                            (if (or (> (length upper-outliers) 0)
                                    (> (length upper-suspected-outliers) 0))
                                upper-inner-fence
                                max))
                           (y-vals (list first-quartile
                                         median
                                         third-quartile
                                         lower-whisker
                                         upper-whisker
                                         lower-outliers
                                         lower-suspected-outliers
                                         upper-suspected-outliers
                                         upper-outliers
                                         min
                                         max))
                           
                           )
                     y-vals
                     
                     )
                   )
                 
                 )
               )
              )
            series-vectors
            )
   
   )
  )


(defun boxplot (series-vectors &key box-widths fill-colors)
  "Constructs a box plots in a window and returns the window.

-returns: <boxplot> object
- arguments: 
  -series-vectors: <list of vector double float> Each vector is transformed into a box plot
  -box-widths: <vector of floats> vector of box widths in units of x-axis, length must match number of elements in series vectors
  -fill-colors: <vector of floats> vector of fill colors, length must match number of elements in series vectors
  "
  (let ((box-widths (if (null box-widths)
                        (make-array (list (length series-vectors))
                                    :element-type 'double-float
                                    :initial-element .1) box-widths))
        (fill-colors (if (null fill-colors)
                         (make-array (list (length series-vectors))
                                     :element-type 'double-float
                                     :initial-element :grey) fill-colors)))
    (assert (= (length series-vectors) (length box-widths)) (box-widths)
          "Length of box-widths must match length of series-vectors")
    (assert (= (length series-vectors) (length fill-colors)) (fill-colors)
            "Length of fill-colors must match length of series-vectors")
    (new-box-plot nil (apply #'prep-boxplot-data series-vectors) :box-widths box-widths
                  :fill-colors fill-colors))
  )
