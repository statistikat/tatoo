# tatoo

[![CRAN status](http://www.r-pkg.org/badges/version/tatoo)](https://cran.r-project.org/package=tatoo)


## Overview

tatoo ("table tools") provides functions to combine data.frames in ways that 
require additional effort in base R, and to add metadata (id, title, ...) 
that can be used for printing and xlsx export. The 'Tatoo_report' class is 
provided as a convenient helper to write several such tables to a workbook, 
one table per worksheet. In addition, the `Tatto_report`
class is provided as a convenient helper to write several such tables to a 
workbook, one table per worksheet.
  
  
## Installation

``` r
# tatoo is available from CRAN
install.packages("tatoo")

# Or you can install the development version from GitHub:
install.packages("devtools")
devtools::install_github("statistikat/tatoo")
```

  
## Example

```R
tag_table(head(cars), tt_meta(table_id = "t1", title = "Data about cars"))
mash_table(head = head(cars), tail = tail(cars), mash_method = "row")
mash_table(head = head(cars), tail = tail(cars), mash_method = "col")
comp_table(head = head(cars), tail = tail(cars))
stack_table(head = head(cars), tail = tail(cars))
```

None of the examples are particularily hard to do in base R, but tatoo provides
functions with a clean interface and nice print and export methods for the 
created objects. Please refer to the package 
[vignette](http://rpubs.com/hoelk/261807) for more examples.
