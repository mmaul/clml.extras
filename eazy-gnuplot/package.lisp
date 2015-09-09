(defpackage #:clml.extras.eazy-gnuplot
  (:use #:cl
        #:clml.time-series.read-data
        #:clml.hjs.read-data)
  (:export :plot-dataset :ignore-warning :plot-series :plot-datasets)
  )
