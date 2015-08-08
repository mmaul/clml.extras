(in-package :clml.extras.eazy-gnuplot)
(defgeneric plot-dataset (dataset y-col &key)
  (:documentation "Generate scatterplot of dataset for dimension name given in y-col"))


 (defun ignore-warning (condition)
   (declare (ignore condition))
   (muffle-warning))

(defmethod plot-dataset ((dataset time-series-dataset) y-col
                         &key
                           range
                           frequencies
                           (xticlabels "xticlabels(1)")
                           xlabel
                           ylabel
                           (title "Time Series Plot")
                           (xtic-interval 2)
                           (series-title "")
                           (terminal '(:wxt :persist))
                           output
                           debug
                           yrange
                           (ytics-font ",4")
                           (xtics-font ",4")
                           (ylabel-font ",6")
                           (xlabel-font ",6")
                           (ylabel-offset "0,0,0")
                           (xlabel-offset "0,0,0"))
   " Plot a time-series-dataset

- return: An value containg the rendered if output is not set containg the plot and the terminal is not interactive
- arguments:
  - dataset          : The time-series-dataset 
  - y-col            : Dimension name in the dataset to plot
  - range            : Two value list containg start and end index of points to plot or nil for all
  - frequencies      : A list of frequencies to plot other wise all
  - *                : everything else follows eazy-gnuplot and gnuplot conventions refer to them respetively for more info
"
  (multiple-value-bind (a b c)
      (handler-bind ((warning #'ignore-warning))
        (eazy-gnuplot:with-plots (*standard-output* :debug debug)
          (eazy-gnuplot:gp-setup
           :xlabel (list (format nil "\"~a\"" (if xlabel xlabel (time-label-name dataset))) :font xlabel-font :offset xlabel-offset)
           :ylabel (list (format nil "\"~a\"" (if ylabel y-label y-col)) :font ylabel-font :offset ylabel-offset)
           :key '(:bottom :right :font ",6")
           :pointsize "0.1px"
           :terminal terminal
           :xtics '(:rotate :by -45)
           :xtics (list :font xtics-font)
           :xtics '(:nomirror)
           :xtics '(:scale 0)
           :ytics (list :font ytics-font)
           :title title
           :output output
           :style '(:line 1 :lw 1)
           :yrange (list (format nil "[~a:~a]" (if yrange  (first yrange) "*") (if yrange (second yrange) "*"))))
          (eazy-gnuplot:plot (lambda ()
                               (loop
                                 for point across (let ((pts (ts-points dataset)))
                                                    (if range (subseq pts (first range) (second range))
                                                        pts))
                                 for i upfrom 0
                                 when (or (not frequencies) (member (ts-p-freq point) frequencies))
                                 do
                                    (format t "~&\"~A\" ~{~A~^ ~}"  (if (= (mod i xtic-interval) 0)
                                                                        (ts-p-label point)
                                                                        "")
                                            (coerce (ts-p-pos point)
                                                    'list))
                                 ) 
                               )
                             :using (list "($0)"
                                          (+ 2 (clml.hjs.read-data:get-dimension-index y-col
                                                                                       (clml.hjs.read-data:dataset-name-index-alist dataset)))
                                          xticlabels)
                             :title series-title
                             :with '(:lines :lc :rgb "#0000BB"))
          
          ))
    (values (remove #\Newline (remove #\Tab a)) b c)
    )
  )
