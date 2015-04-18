<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. CL Machine-Learning Extras</a>
<ul>
<li><a href="#sec-1-1">1.1. Author(s):</a></li>
<li><a href="#sec-1-2">1.2. Installation</a></li>
<li><a href="#sec-1-3">1.3. Installation Notes</a>
<ul>
<li><a href="#sec-1-3-1">1.3.1. Obtaining code</a></li>
<li><a href="#sec-1-3-2">1.3.2. Installing</a>
<ul>
<li><a href="#sec-1-3-2-1">1.3.2.1. For Quicklisp <b>**</b></a></li>
<li><a href="#sec-1-3-2-2">1.3.2.2. For ASDF3 only (Non quicklisp users)</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-1-4">1.4. Usage</a>
<ul>
<li><a href="#sec-1-4-1">1.4.1. CLML.ANA</a></li>
<li><a href="#sec-1-4-2">1.4.2. CLML.R-DATASETS</a></li>
</ul>
</li>
<li><a href="#sec-1-5">1.5. Building Documentation</a></li>
<li><a href="#sec-1-6">1.6. API Documentation</a></li>
</ul>
</li>
<li><a href="#sec-2">2. Package: <code>clml.ana.plotting</code></a>
<ul>
<li><a href="#sec-2-1">2.1. Description</a></li>
<li><a href="#sec-2-2">2.2. External Symbols</a></li>
</ul>
</li>
<li><a href="#sec-3">3. Package: <code>clml.cl-plplot</code></a>
<ul>
<li><a href="#sec-3-1">3.1. Description</a></li>
<li><a href="#sec-3-2">3.2. External Symbols</a>
<ul>
<li><a href="#sec-3-2-1">3.2.1. External Functions</a>
<ul>
<li><a href="#sec-3-2-1-1">3.2.1.1. Inherited Function: <code>boxplot</code></a>
<ul>
<li><a href="#sec-3-2-1-1-1">3.2.1.1.1. Syntax</a></li>
<li><a href="#sec-3-2-1-1-2">3.2.1.1.2. Description</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-4">4. Package: <code>clml.r-datasets</code></a>
<ul>
<li><a href="#sec-4-1">4.1. Description</a></li>
<li><a href="#sec-4-2">4.2. Description</a></li>
<li><a href="#sec-4-3">4.3. Other uses</a></li>
<li><a href="#sec-4-4">4.4. External Symbols</a>
<ul>
<li><a href="#sec-4-4-1">4.4.1. External Functions</a>
<ul>
<li><a href="#sec-4-4-1-1">4.4.1.1. Inherited Function: <code>dataset-documentation</code></a>
<ul>
<li><a href="#sec-4-4-1-1-1">4.4.1.1.1. Syntax</a></li>
<li><a href="#sec-4-4-1-1-2">4.4.1.1.2. Description</a></li>
</ul>
</li>
<li><a href="#sec-4-4-1-2">4.4.1.2. Inherited Function: <code>get-dataset</code></a>
<ul>
<li><a href="#sec-4-4-1-2-1">4.4.1.2.1. Syntax</a></li>
<li><a href="#sec-4-4-1-2-2">4.4.1.2.2. Description</a></li>
</ul>
</li>
<li><a href="#sec-4-4-1-3">4.4.1.3. Inherited Function: <code>get-r-dataset-directory</code></a>
<ul>
<li><a href="#sec-4-4-1-3-1">4.4.1.3.1. Syntax</a></li>
<li><a href="#sec-4-4-1-3-2">4.4.1.3.2. Description</a></li>
</ul>
</li>
<li><a href="#sec-4-4-1-4">4.4.1.4. Inherited Function: <code>inventory</code></a>
<ul>
<li><a href="#sec-4-4-1-4-1">4.4.1.4.1. Syntax</a></li>
<li><a href="#sec-4-4-1-4-2">4.4.1.4.2. Description</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


# CL Machine-Learning Extras

CLML. EXTRAS is a system to extensions to the CLML
(CL Machine Learning Library). This repository contains extensions
to CLML and interfaces to other systems.

The CLML library can be found at [CLML](https://github.com/mmaul/clml) github repository.

## Author(s):

-   Mike Maul

## Installation

## Installation Notes

### Obtaining code

Code can be obtained by one of the following methods:
-   Clone this repository with:

    git clone https://github.com/mmaul/clml.extras.git

Or download zip archive at

    https://github.com/mmaul/clml.extras/archive/master.zip

clml.extras requires clml which can be found at [<https://github.com/mmaul/clml>](https://github.com/mmaul/clml)

### Installing

#### For Quicklisp **\*\***

1.  Place code in `~/quicklisp/local-projects`
2.  Start LISP and enter `(ql:quickload :clml.extras)`

#### For ASDF3 only (Non quicklisp users)

1.  Place in a location on your ASDF search path path such as `~/common-lisp`
2.  Start LISP and enter `(asdf:load-system :clml.extras)`

## Usage

This library contains the following extensions:
  +clml.ana.plotting : Compatibility layer between CLML and CL-ANA
    CL-ANA is a gnuplot wrapper and provides complimentary functionality to
    CLML. of particular not is the lispy gnuplot wrapper and
    histograms.
  +clml.r-datasets
    Provides access to datasets included with the R programming
    language as CLML datasets.

### CLML.ANA

Below demonstrates using CL-ANA's gluplot with CLML datasets and using
data from CLML datasets to feed CL-ANA's histograms.

    (require :plotting)
    (require :clml.ana.plotting)
    (setf *syobu* (hjs.learn.read-data:read-data-from-file 
               (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/syobu.csv")
               :type :csv :csv-type-spec '(string integer integer integer integer)))
    #<HJS.LEARN.READ-DATA:UNSPECIALIZED-DATASET >
    DIMENSIONS: 種類 | がく長 | がく幅 | 花びら長 | 花びら幅
    TYPES:      UNKNOWN | UNKNOWN | UNKNOWN | UNKNOWN | UNKNOWN
    NUMBER OF DIMENSIONS: 5
    DATA POINTS: 150 POINTS
    
    PLOTTING> (setf mydata (hjs.learn.read-data::choice-dimensions '("がく長" "花びら幅") *syobu*))
    #(#(51 2) #(49 2) #(47 2) #(46 2) #(50 2) #(0 4) #(46 3) #(50 2) #(44 2)
      #(49 1) #(54 2) #(48 2) #(48 1) #(43 1) #(58 2) #(57 4) #(0 4) #(51 3)
      #(57 3) #(51 3) #(54 2) #(51 4) #(46 2) #(51 5) #(48 2) #(50 2) #(50 4)
      #(52 2) #(52 2) #(47 2) #(48 2) #(54 4) #(52 1) #(55 2) #(49 2) #(50 2)
      #(55 2) #(49 1) #(44 2) #(51 2) #(50 3) #(45 3) #(44 2) #(50 6) #(51 4)
      #(48 3) #(51 2) #(46 2) #(53 2) #(50 2) #(70 14) #(64 15) #(69 15) #(55 13)
      #(65 15) #(57 13) #(63 16) #(49 10) #(66 13) #(52 14) #(50 10) #(59 15)
      #(60 10) #(61 14) #(56 13) #(67 14) #(56 15) #(58 10) #(62 15) #(56 11)
      #(59 18) #(61 13) #(63 15) #(61 12) #(64 13) #(66 14) #(68 14) #(67 17)
      #(60 15) #(57 10) #(55 11) #(55 10) #(58 12) #(60 16) #(54 15) #(60 16)
      #(67 15) #(63 13) #(56 13) #(55 13) #(55 12) #(61 14) #(58 12) #(50 10)
      #(56 13) #(57 12) #(57 13) #(62 13) #(51 11) #(57 13) #(63 25) #(58 19)
      #(71 21) #(63 18) #(65 22) #(76 21) #(49 17) #(73 18) #(67 18) #(72 25)
      #(65 20) #(64 19) #(68 21) #(57 20) #(58 24) #(64 23) #(65 18) #(77 22)
      #(77 23) #(60 15) #(69 23) #(56 20) #(77 20) #(63 18) #(67 21) #(72 18)
      #(62 18) #(61 18) #(64 21) #(72 16) #(74 19) #(79 20) #(64 22) #(63 15)
      #(61 14) #(77 23) #(63 24) #(64 18) #(60 18) #(69 21) #(67 24) #(69 23)
      #(58 19) #(68 23) #(67 25) #(67 23) #(63 19) #(65 20) #(62 23) #(59 18))
    CL-USER> ; By default 2d vector is plotted as a list of points
    CL-USER> (plotting:draw mydata
                :plot-args '(:x-range (0 . 80)
                         :y-range (0 . 80)))
    CL-USER> ; We can also plot as lines
    CL-USER> (plotting:draw (plotting:line mydata :style :lines))
    NIL 
    CL-USER> ; We can combine multiple lines on a plot
    NIL
    CL-USER> (plotting:draw (plotting:plot2d (list (plotting:line mydata :title "points") 
                 (plotting:line mydata :title "lines" :style "lines"))))
    NIL
    CL-USER> ; Using CL-ANA histograms with CLML Distributions
    CL-USER> (defparameter vv (clml.statistics:rand-n 
                 (clml.statistics:standard-normal-distribution) 100))
    CL-USER> (defparameter *contiguous-hist*
              (histogram:make-contiguous-hist
               '((:name "x" :low -4d0 :high 4d0 :nbins 10)
                 (:name "y" :low 0d0 :high 1d0 :nbins 10))
               :empty-bin-value 0d0
               :default-increment 1))
    CL-USER> (loop
               for v in vv
               do (histogram:hist-insert *contiguous-hist* v))
    CL-USER> (plotting:draw *contiguous-hist* )
    
    NIL

### CLML.R-DATASETS

    EXTRAS> (defparameter dd (get-r-dataset-directory))
    DD
    EXTRAS> (inventory dd)
    Package                   Item                      Title                     
    ------------------------- ------------------------- ------------------------- 
    datasets                  AirPassengers             Monthly Airline Passenger Numbers 1949-1960 
    ...
    datasets                  cars                      Speed and Stopping Distances of Cars 
    EXTRAS> (defparameter ds (get-dataset dd "datasets" "cars"))
    EXTRAS> (head-points ds)
    #(#("1" "4" "2") #("2" "4" "10") #("3" "7" "4") #("4" "7" "22") #("5" "8" "16"))
    EXTRAS> (setq ds (get-dataset dd "datasets" "cars" :csv-type-spec '(integer integer integer)))
    #<UNSPECIALIZED-DATASET >
    DIMENSIONS:  | speed | dist
    TYPES:      UNKNOWN | UNKNOWN | UNKNOWN
    NUMBER OF DIMENSIONS: 3
    DATA POINTS: 50 POINTS
    EXTRAS> (head-points ds)
    #(#(1 4 2) #(2 4 10) #(3 7 4) #(4 7 22) #(5 8 16))

## Building Documentation

CLML.EXRTAS uses the a modified version of the CLOD (used in CLML) package for it's
dcumentation system. Specific details of using clod can be found
most easily in the [clod](http://quickdocs.org/clod/api) api documentation] at [quickdocs](http://)

    (ql:quickload :clml.extras.docs :verbose t)
    (in-package :clml.extras)
    (clml.extras.docs:generate-clml-api-docs)

Documentation is in the form of Org files where one Org file per package is placed in
**docs/api**.  A package index file containing Org INCLUDE
directives that include 
Org files generated by the form **generate-api-docs** are  placed
in **docs/api/index.org**.

The README.md file is generated by the org-mode export function.
Which can be done by opening the README.org file in emacs and
entering org-mode and using the export function `C-c C-e` and
selecting the markdown export option as shown below.

    M-x org-md-export-as-markdown
    C-x-C-w README.md

The CMLM manual and API documentation can be exported to the desired 
format by opening the docs/clml-manual.org and using the org-mode
export `C-c C-e` cord.

## API Documentation

# Package: `clml.ana.plotting`

-   Uses:
    common-lisp, plotting
-   Used by:
    clml.extras

## Description

Interoperability for CL-ANA plotting

## External Symbols

# Package: `clml.cl-plplot`

-   Uses:
    common-lisp, clml.statistics, cl-plplot
-   Used by:
    common-lisp-user, clml.extras

## Description

This package provides a enhancements to cl-plplot and wrappers to clml-plplot functions.

## External Symbols

### External Functions

---

#### Inherited Function: `boxplot`

##### Syntax

    (boxplot series-vectors &key box-widths fill-colors)

##### Description

Constructs a box plots in a window and returns the window.

-returns: <boxplot> object

-   arguments: 
    -series-vectors: <list of vector double float> Each vector is transformed into a box plot
    -box-widths: <vector of floats> vector of box widths in units of x-axis, length must match number of elements in series vectors
    -fill-colors: <vector of floats> vector of fill colors, length must match number of elements in series vectors

# Package: `clml.r-datasets`

-   Uses:
    common-lisp, drakma, clml.utility.data, clml.hjs.read-data
-   Used by:
    clml.extras

## Description

## Description

Makes datasets included with the R language distribution available as clml datasets.
R datasets are obtained csv files on Vincent Centarel's github repository.
More information on these datasets can be found at <http://vincentarelbundock.github.com/Rdatasets>

Because type information is not included it may be necessary to provide a type specification
for the columns in the csv file.

    (ql:quickload :clml.r-datasets)
    (defparameter dd (get-r-dataset-directory))
    (inventory dd)
      Package                   Item                      Title                     
      ------------------------- ------------------------- ------------------------- 
      datasets                  AirPassengers             Monthly Airline Passenger Numbers 1949-1960 
      datasets                  BJsales                   Sales Data with Leading Indicator 
      datasets                  BOD                       Biochemical Oxygen Demand 
    (dataset-documentation  dd  "datasets" "BOD")
      R: Biochemical Oxygen Demand
      BODR Documentation
      Biochemical Oxygen Demand
        Description
          The BOD data frame has 6 rows and 2 columns giving the
          biochemical oxygen demand versus time in an evaluation of water
          quality.
          ...
    
    (get-dataset dd "datasets" "BOD")
      #<UNSPECIALIZED-DATASET >
      DIMENSIONS:  | Time | demand
      TYPES:      UNKNOWN | UNKNOWN | UNKNOWN
      NUMBER OF DIMENSIONS: 3
      DATA POINTS: 6 POINTS

## Other uses

This package can also be used as a tool for sharing or distributing bundles of datasets.
To do this a csv file which provides the directory of data sets must be made availabe
via a URL. The csv file MUST comply to the following format:
A header with following collumns
-   Package : package
-   Item    : dataset name
-   Title   : Brief Description of dataset
-   csv     : URL where dataset is available
-   doc     : URL with documentation describing the dataset

The the contents of the file pointed to by doc `doc` can be plaintext of HTML.
If it is HTML the HTML tags will be stripped and what ever whitespace formatting will
be used. This field can be empty however the `inventory` method will be un available if it is

## External Symbols

### External Functions

---

#### Inherited Function: `dataset-documentation`

##### Syntax

    (dataset-documentation dataset-directory package name &key stream (stream t))

##### Description

Outputs documention for the R dataset to the specified stream if no stream supplied defaults to console
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
  -stream: <string> <key> <optional> specify output stream for documentation 

---

#### Inherited Function: `get-dataset`

##### Syntax

    (get-dataset dataset-directory package name &key csv-type-spec (csv-header-p t)
                 (missing-value-check t))

##### Description

Returns the dataset specified by the `package` and `name`
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
  -csv-type-spec: <list> column type specifier list
  -csv-header-p: <bool default t> if true first line of CSV is header
  -missing-values-check <boolean default t> Check for presence of missing values

Returns unspecialized dataset containing contents of CSV specified by the package and name.
Because type information is not included in the CSV nor in the dataset directory it will
probably in most cases be necessary to specify `csv-type-spec`. If `csv-type-spec` is not
set the columns wil be read as strings. You could of course manipulate the dataset later.
It is in most cases better to specify the `csv-type-spec`. The `csv-type~spec` should be a
list containing one of the follwong symbols:
  keyword symbol pathname integer double-float single-float float 
  number t nil string
The values t or nil in the spec will cause the column to be interpeted as a string.

Missing values are defined by a column value of NA or the empty string. Missing values will
cause the value to be represented by the keyword :na in the dataset. For R datasets it is
not necessary to set `csv-header-p` `missing-values-check`.

Example:

    ; grab the data and see what the types should be
    CL-USER> (head-points (get-dataset dd "datasets" "BOD"))
    #(#("1" "1" "8.3") #("2" "2" "10.3") #("3" "3" "19") #("4" "4" "16")
      #("5" "5" "15.6") #("6" "7" "19.8"))
    ; Looks like '(integer integer double-float) will do
    CL-USER> (head-points (get-dataset dd "datasets" "BOD" :csv-type-spec '(integer integer double-float)))
    #(#(1 1 8.3) #(2 2 10.3) #(3 3 19.0) #(4 4 16.0) #(5 5 15.6))

---

#### Inherited Function: `get-r-dataset-directory`

##### Syntax

    (get-r-dataset-directory &optional (url))

##### Description

-returns: <dataset-directory> object containg directory of available R datasets
-   arguments:
    -url: <string> Optional URL containing the location of the R dataset directory. Only needed if a custom directory is needed.

---

#### Inherited Function: `inventory`

##### Syntax

    (inventory dataset-directory &key stream (stream t))

##### Description

Outputs R packages, datasets and description available datasets in inventory
-return: nil
-arguments:
  -package: <dataset-directory>  datasets
  -stream: <string> <key> <optional> specify output stream for documentation