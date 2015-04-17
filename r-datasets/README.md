<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Package: <code>clml.r-datasets</code></a></li>
</ul>
</div>
</div>



# Package: `clml.r-datasets`

-   Uses:
    common-lisp, drakma, clml.utility.data, clml.hjs.read-data
-   Used by:
    None.

## Description

## Description

Makes datasets included with the R language distribution available as clml datasets.
R datasets are obtained csv files on Vincent Centarel's github repository.
More information on these datasets can be found at <http://vincentarelbundock.github.com/Rdatasets>

Because type information is not included it may be necessary to provide a type specification
for the columns in the csv file.

## Example

\#+BEGIN<sub>SRC</sub> lisp
CL-USER> (ql:quickload :clml.r-datasets)
R-DATASETS> (defparameter dd (get-r-dataset-directory))
R-DATASETS> (inventory dd)
R-DATASETS> (inventory dd)
Package                   Item                      Title                     
-&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014; -&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014; -&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014; 
datasets                  AirPassengers             Monthly Airline Passenger Numbers 1949-1960 
datasets                  BJsales                   Sales Data with Leading Indicator 
datasets                  BOD                       Biochemical Oxygen Demand 
&#x2026;

R-DATASETS>(dataset-documentation dd 

## External Symbols

### External Functions

---

#### External Function: `dataset-documentation`

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

#### External Function: `get-dataset`

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
\\#+BEGIN<sub>SRC</sub> lisp
; grab the data and see what the types should be
CL-USER> (head-points (get-dataset dd "datasets" "BOD"))
\\#(#("1" "1" "8.3") #("2" "2" "10.3") #("3" "3" "19") #("4" "4" "16")
  #("5" "5" "15.6") #("6" "7" "19.8"))
; Looks like '(integer integer double-float) will do
CL-USER> (head-points (get-dataset dd "datasets" "BOD" :csv-type-spec '(integer integer double-float)))
\\#(#(1 1 8.3) #(2 2 10.3) #(3 3 19.0) #(4 4 16.0) #(5 5 15.6))

---

#### External Function: `get-r-dataset-directory`

##### Syntax

    (get-r-dataset-directory &optional (url))

##### Description

-returns: <dataset-directory> object containg directory of available R datasets
-   arguments:
    -url: <string> Optional URL containing the location of the R dataset directory. Only needed if a custom directory is needed.

---

#### External Function: `inventory`

##### Syntax

    (inventory dataset-directory &key stream (stream t))

##### Description

Outputs R packages, datasets and description available datasets in inventory
-return: nil
-arguments:
  -package: <dataset-directory>  datasets
  -stream: <string> <key> <optional> specify output stream for documentation