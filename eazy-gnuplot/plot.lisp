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
           :ylabel (list (format nil "\"~a\"" (if ylabel ylabel y-col)) :font ylabel-font :offset ylabel-offset)
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


(defmethod plot-dataset ((dataset specialized-dataset) y-col
                         &key
                           x-col
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
           :ylabel (list (format nil "\"~a\"" (if ylabel ylabel y-col)) :font ylabel-font :offset ylabel-offset)
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
                                 for point across (let ((pts (dataset-points dataset)))
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

(defmethod plot-dataset ((dataset numeric-dataset) y-col
                         &key
                           x-col
                           range
                           (xticlabels "xticlabels(1)")
                           xlabel
                           ylabel
                           (title "Time Series Plot")
                           (xtic-interval 2)
                           (series-title "")
                           (terminal '(:wxt :persist))
                           output
                           debug
                           xrange
                           yrange
                           (ytics-font ",4")
                           (xtics-font ",4")
                           (ylabel-font ",6")
                           (xlabel-font ",6")
                           (ylabel-offset "0,0,0")
                           (xlabel-offset "0,0,0")
                           (style '(:line 1 :lw 1))
                           (with '(:lines :lc :rgb "#0000BB")) )
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
           :xlabel (list (format nil "\"~a\"" (if xlabel xlabel x-col)) :font xlabel-font :offset xlabel-offset)
           :ylabel (list (format nil "\"~a\"" (if ylabel ylabel y-col)) :font ylabel-font :offset ylabel-offset)
           :key '(:bottom :right :font ",6")
           :pointsize "0.1px"
           :terminal terminal
           ;:xtics '(:rotate :by -45)
           :xtics (list :font xtics-font)
           :xtics '(:nomirror)
           ;:xtics '(:scale 0)
           :ytics (list :font ytics-font)
           :title title
           :output output
           :style style
           :yrange (list (format nil "[~a:~a]" (if yrange  (first yrange) "*") (if yrange (second yrange) "*")))
           :xrange (list (format nil "[~a:~a]" (if xrange  (first xrange) "*") (if xrange (second xrange) "*"))))
          (eazy-gnuplot:plot (lambda ()
                               (loop
                                 for point across (let ((pts (dataset-points dataset)))
                                                    (if range (subseq pts (first range) (second range))
                                                        pts))
                                 for i upfrom 0
                                 
                                 do
                                    (format t "~&\"~A\" ~{~A~^ ~}" ""  
                                            (coerce point
                                                    'list))
                                 ) 
                               )
                             :using
                             (list
                              (if x-col
                                  (+ 2 (clml.hjs.read-data:get-dimension-index x-col
                                          (clml.hjs.read-data:dataset-name-index-alist dataset)))
                                  "($0)")
                                        
                              (if y-col (+ 2 (clml.hjs.read-data:get-dimension-index y-col
                                          (clml.hjs.read-data:dataset-name-index-alist dataset)))
                                  "($0)")
                              ;xticlabels
                              )
                             :title series-title
                             :with with)
          
          ))
    (values (remove #\Newline (remove #\Tab a)) b c)
    )
  )
                                        ;[0:1] [0:1]

(defmethod plot-dataset-data ((dataset numeric-dataset) y-col
                         &key
                           x-col
                           range
                           (xticlabels "xticlabels(1)")
                           xlabel
                           ylabel
                           (title "Time Series Plot")
                           (xtic-interval 2)
                           (series-title "")
                           (terminal '(:wxt :persist))
                           output
                           debug
                           xrange
                           yrange
                           (ytics-font ",4")
                           (xtics-font ",4")
                           (ylabel-font ",6")
                           (xlabel-font ",6")
                           (ylabel-offset "0,0,0")
                           (xlabel-offset "0,0,0")
                           (style '(:line 1 :lw 1))
                           (with '(:lines :lc :rgb "#0000BB")) )
   " Plot a time-series-dataset

- return: An value containg the rendered if output is not set containg the plot and the terminal is not interactive
- arguments:
  - dataset          : The time-series-dataset 
  - y-col            : Dimension name in the dataset to plot
  - range            : Two value list containg start and end index of points to plot or nil for all
  - frequencies      : A list of frequencies to plot other wise all
  - *                : everything else follows eazy-gnuplot and gnuplot conventions refer to them respetively for more info
"

  (eazy-gnuplot:plot (lambda ()
                       (loop
                         for point across (let ((pts (dataset-points dataset)))
                                            (if range (subseq pts (first range) (second range))
                                                pts))
                         for i upfrom 0
                         
                         do
                            (format t "~&\"~A\" ~{~A~^ ~}" ""  
                                    (coerce point
                                            'list))
                         ) 
                       )
                     :using
                     (list
                      (if x-col
                          (+ 2 (clml.hjs.read-data:get-dimension-index x-col
                                                                       (clml.hjs.read-data:dataset-name-index-alist dataset)))
                          "($0)")
                      
                      (if y-col (+ 2 (clml.hjs.read-data:get-dimension-index y-col
                                                                             (clml.hjs.read-data:dataset-name-index-alist dataset)))
                          "($0)")
                                        ;xticlabels
                      )
                     :title title
                     :with with)
  )

(defun test-plot ( &key debug (path "/tmp/wtf") (term '(:wxt :persist)))
  (eazy-gnuplot:with-plots (*standard-output* :debug debug)
    (eazy-gnuplot:gp-setup :xlabel "x-label"       ; strings are "quoted"
              :ylabel "y-label"
              :terminal term          ; keyword/symbols are not quoted
                                        ; (but not escaped)
              :key '(:bottom :right :font "Times New Roman, 6")
              ;; list contents are recursively quoted
              ;; then joined by a space
              :pointsize "0.4px")
    
    (eazy-gnuplot:func-plot "sin(x)" :title "super sin curve!")
    (eazy-gnuplot:plot (lambda ()
            (format t "~&0 0")
            (format t "~&1 1"))
          :using '(1 2)
          :title "1"
          :with '(:linespoint))
    (eazy-gnuplot:plot (lambda ()
            (format t "~&0 1")
            (format t "~&1 0"))
          :using '(1 2)
          :title "2"
          :with '(:lines))))



(defun plot-series (series
                    &key debug
                      (term '(:wxt :persist))
                      (plot-title "Series")
                      (series-title "Y ")
                      (ylabel "Y ")
                      (xlabel "X ")
                      (output "series.out"))
  (handler-bind ((warning #'ignore-warning))  
  (eazy-gnuplot:with-plots (*standard-output* :debug debug)
    (eazy-gnuplot:gp-setup
     :xlabel xlabel
     :title plot-title
     :ylabel ylabel
     :terminal term
     :output output
     :key '(:bottom :right :font ", 6")
     :pointsize "0.4px")
    (eazy-gnuplot:plot (lambda ()
                         (loop for x from 1 upto (length series) for y across series do (format t "~&~A ~A" x y))
                         )
          :using '(1 2)
          :title series-title
          :with '(:linespoint))
)))

(defun plot-datasets (dataset-plot-defs &rest args
                      &key debug
                        (term '(:wxt :persist))
                        (path "/tmp/wtf")
                        (key '(:bottom :right :font "Times New Roman, 6"))
                        (xlabel "X")
                        (ylabel "Y")
                      &allow-other-keys)
  (eazy-gnuplot:with-plots (*standard-output* :debug debug)
    (apply #'eazy-gnuplot:gp-setup (append (list :xlabel xlabel
                                                 :ylabel ylabel
                                                 :output path
                                                 :terminal term
                                                 :key key
                                                 :pointsize "0.4px") args) )
    (loop for dataset-plot-def in dataset-plot-defs
          do (apply #'plot-dataset-data dataset-plot-def))
    ))
