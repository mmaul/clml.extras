<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Package: <code>clml.cl-plplot</code></a></li>
</ul>
</div>
</div>



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