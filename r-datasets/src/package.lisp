;-*- coding: utf-8 -*-

(defpackage :clml.r-datasets
  (:use :common-lisp :drakma :clml.utility.data :clml.hjs.read-data) 
  (:nicknames :r-datasets)
  (:export 
           #:get-r-dataset-directory
           #:get-dataset 
           #:dataset-documentation
           #:inventory
           )
  (:documentation
   "
* Description
Makes datasets included with the R language distribution available as clml datasets.
R datasets are obtained csv files on Vincent Centarel's github repository.
More information on these datasets can be found at http://vincentarelbundock.github.com/Rdatasets

Because type information is not included it may be necessary to provide a type specification
for the columns in the csv file.

#+INCLUDE: "./docs/examples/r-datasets.org" :minlevel 2

* Other uses
This package can also be used as a tool for sharing or distributing bundles of datasets.
To do this a csv file which provides the directory of data sets must be made availabe
via a URL. The csv file MUST comply to the following format:
A header with following collumns
  - Package : package 
  - Item    : dataset name
  - Title   : Brief Description of dataset
  - csv     : URL where dataset is available
  - doc     : URL with documentation describing the dataset

The the contents of the file pointed to by doc ~doc~ can be plaintext of HTML.
If it is HTML the HTML tags will be stripped and what ever whitespace formatting will
be used. This field can be empty however the ~inventory~ method will be un available if it is

 
")
  )

