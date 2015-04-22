;;;; k-means.lisp
;;;; k-means in clml
(defpackage #:clml.examples.k-means
  (:use #:cl #:clml.hjs.read-data :clml.hjs.k-means :clml.utility.data))

(in-package #:clml.examples.k-means)
(setq *read-default-float-format* 'double-float)
;;;; This tutorial illistrates how to use k-means in CLML
;;;; Besides how to use k-means One thing this illistrates is working
;;;; with diverse data. The datasets in the UCI Machine learning archive 
;;;; do not appear to have header columns. So we have to refer to the <dataset
;;;; name>.name file located in the same directory as the .data file
;;;; In this case:
;;;;   (http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names)
;;;; Suffice it to say after a brief read of the .names file you can
;;;; see we extracted the column names.
;;;; Another thing that is necessary to figure out is the types of the
;;;; data. This is something that generally needs to be done manually
;;;; or with knowldege of the dataset. 
;;;; Typically you might look at the data description, or browse the
;;;; datafile. If you wanted to do this from with in CLML you might.
(format t "~A" (head-points (read-data-from-file
                                           (fetch "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
                                           :type :csv)))
;;;; #(#(1 13.2 1.78 2.14 11.2 100 2.65 2.76 .26 1.28 4.38 1.05 3.4 1050)
;;;;   #(1 13.16 2.36 2.67 18.6 101 2.8 3.24 .3 2.81 5.68 1.03 3.17 1185)
;;;;  #(1 14.37 1.95 2.5 16.8 113 3.85 3.49 .24 2.18 7.8 .86 3.45 1480)
;;;;  #(1 13.24 2.59 2.87 21 118 2.8 2.69 .39 1.82 4.32 1.04 2.93 735)
;;;;  #(1 14.2 1.76 2.45 15.2 112 3.27 3.39 .34 1.97 6.75 1.05 2.85 1450))

;;;; So is is all numeric data, The first colum from the dataset
;;;; definition is the class so we don't want to give that to our
;;;; k-means function as that would be cheating. But lets just call
;;;; all the collumns of type double float

;;;; fetch downloads the file (or give you a path to the cached copy)
;;;; read-data, parses the csv, applies the specified column names and
;;;; types to the columns and yeilds an unspecialized dataset.
;;;; The k-means function requires a numeric-dataset which is provided
;;;; by the pick-and-specialize data

(defparameter wine
  (pick-and-specialize-data 
   (read-data-from-file
    (fetch "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
    :type :csv
    :csv-type-spec '(integer
                     double-float	double-float	double-float
                     double-float	double-float    double-float
                     double-float double-float    double-float
                     double-float double-float    double-float
                     double-float)
    :csv-header-p ( list "Class"
                         "Alcohol"            "Malic acid"           "Ash"
                         "Alcalinity of ash"  "Magnesium"            "Total phenols"
                         "Flavanoids"         "Nonflavanoid phenols" "Proanthocyanins"
                         "Color intensity"    "Hue"                   "OD280/OD315 of diluted wines"
                         "Proline")
    )
   :range '(1 2 3 4 5 6 7 8 9 10 11 12 13) :data-types (make-list 13 :initial-element :numeric )))

;;;; So since we are exploring the data we want our results to be
;;;; repeatable so we will pick a fixed random state for the k-means
;;;; function, with the make-random-state-with-seed function.
(let ((state (make-random-state-with-seed 1234)))
;;;; Run k-means with the defaults. workspace is the k-means run
;;;; information and cluster assignment. Since the k-means is doing
;;;; the standardisation of the data for us the cluster assignment is
;;;; using standardidzed values. The table value contains a hash table
;;;; that maps the standardized values back to the original values
  (multiple-value-bind (workspace table)
           (k-means 3 wine  :standardization t :random-state state)
      ;;;; Now lets see how we did.
      (print workspace  )
      ;;;; #<CLML.HJS.K-MEANS::PROBLEM-WORKSPACE 3 Clusters (ID size):
      ;;;;   ((0 51) (1 61) (2 66)) {10097B0003}>
      ;;;; So the cluster distribution is ((0 51) (1 61) (2 66))
      ;;;; from the dataset description in the .names file the
      ;;;; actual distribution is: class 1 59
	  ;;;;                         class 2 71
	  ;;;;                         class 3 48
      ;;;; Not too bad
      ;;;; So what data is available from our clustering
      ;;;; The cluster centroids:
      (get-cluster-centroids workspace)
      ;;;; ((0
      ;;;;  . #(0.16444358896995892 0.8690954472537682 0.18637259499845618
      ;;;;      0.5228924410811502 -0.07526046614302336 -0.9765754839938489
      ;;;;      -1.2118292123465357 0.724021159373092 -0.7775131171790667
      ;;;;      0.9388902409225227 -1.1615121632678649 -1.2887761433394702
      ;;;;      -0.40594284143656034))
      ;;;;      
      ;;;;  (1
      ;;;;  . #(0.8756272418170851 -0.30371957168664576 0.3180446264140999
      ;;;;     -0.6626543948232362 0.5632992471916901 0.8740398997645874
      ;;;;      0.9409846247986108 -0.5839425807469174 0.5801464167356171
      ;;;;      0.1667181285145495 0.48236743459432646 0.7648958114081033
      ;;;;      1.1550887659006268))
      ;;;;  (2
      ;;;;  . #(-0.9363618907319501 -0.3908632414705565 -0.4379655235785164
      ;;;;      0.20840005437998074 -0.4624692470514986 -0.0531982454841447
      ;;;;      0.06671557146906414 -0.019766389431300546 0.06460965992817365
      ;;;;      -0.8795940625217612 0.45170767903647707 0.28892331536998617
      ;;;;      -0.753898936464753))
      ;;;;
      ;;;;  Cluster points
      (print (subseq (get-cluster-points workspace 0) 0 2))
      ;;;; #(#(1.3911617440421347 1.5787117630892988 1.361367967705916 1.4987155594909907
      ;;;;     -0.26196935775916486 -0.3916464788342626 -1.2707199546448487
      ;;;;     1.5921313718328312 -0.4208878242554346 1.78662613110848 -1.5200903793122311
      ;;;;     -1.4249282134945676 -0.5934862576893309)
      ;;;;   #(0.2086431208420219 0.22705327972312908 0.012696271660310293
      ;;;;     0.151234177571265 1.4184106667861087 -1.030776189881695 -1.350811364170554
      ;;;;     1.351077174906624 -0.22870070674327575 1.8297614498435204
      ;;;;     -1.5638403530401324 -1.396758819669375 0.2956638034293828))
      ;;;;
      ;;;; These are the standardized values to get the original
      ;;;; values back we can use the table hashtable each vector to
      ;;;; look up the original value
      (print (gethash (elt (get-cluster-points workspace 0) 0) table))
      ;;;; #(14.13 4.1 2.74 24.5 96.0 2.05 0.76 0.56 1.35 9.2 0.61 1.6 560.0)
      )
  )

