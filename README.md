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
<li><a href="#sec-1-3-2">1.3.2. Installing</a></li>
</ul>
</li>
<li><a href="#sec-1-4">1.4. Usage</a>
<ul>
<li><a href="#sec-1-4-1">1.4.1. CLML.ANA</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


# CL Machine-Learning Extras

CL Machine-Learning Extras is a repository to extensions to the CLML
(CL Machine Learning Library). This repository will contain extensions
that may not organically fit the CLML distribution and experimental
extensions that may later be incorporated into the CLML distribution.

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

### Installing

1.  For Quicklisp **\*\***

    1.  Place code in `~/quicklisp/local-projects`
    2.  Start LISP and enter `(ql:quickload :clml.extras)`

2.  For ASDF3 only (Non quicklisp users)

    1.  Place in a location on your ASDF search path path such as `~/common-lisp`
    2.  Start LISP and enter `(asdf:load-system :clml.extras)`

## Usage

This library contains the following extensions:
  +clml.ana : Compatibility layer between CLML and CL-ANA
    CL-ANA is blah bla and provides complimentary functionality to
    CLML. of particular not is the lispy gnuplot wrapper and
    histograms.

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