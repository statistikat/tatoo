# tatoo 1.1.2

* **breaking** add required arguments of the generic to tatoo specific 
  `as.data.frame` and `as.data.table` methods to make them compatible with new 
  R-devel. This means you have to call these functions with named arguments now.


# tatoo 1.1.1

* removed **purrr** and **rlang** dependencies
* `table_id` is no longer pasted to the title when creating excel output. If you
  require the old behavior set `.print_table_id = TRUE`
* small updates to some tests for compat with data.table 1.12.2 
  (https://github.com/statistikat/tatoo/pull/5)


# tatoo 1.1.0

* Add Named Regions for table parts Excel Sheets to make formatting easier
* Added `walk_regions()` and `map_regions()` to manipulate cells in named
  regions within a `Workbook` (e.g. apply formatting to them)


# tatoo 1.0.10

* Bugfix release 
* Fixed faulty example code for `open_file()`
* Removed default arguments of `save_*()` functions (they wrote to the users
  home directory by default)


# tatoo 1.0.9

* Convert `Tatoo_table` objects to Latex code or save them directly to `.pdf`
  with `as_latex()` and `save_pdf()` (experimental).
* Added `open_file()` utility to open files with external programs.
* Added `view_pdf()` and `view_xlsx()` for directly viewing tables in a .pdf
  viewer or spreadsheet program
* `save_xlsx()` now returns the path to the saved workbook, rather than the
  workbook object
* Various small fixes related to `print()` methods


# tatoo 1.0.8

* Support color themes for printing via colt and crayon


# tatoo 1.0.7

* Minor tweaks for compatibility with purrr 0.2.2
* `as_Composite_table.data.table()` converts data.tables to Composite tables, 
  based  on separators in the column names (e.g fruit.apple, fruit.banana)
* Improved README / vignette / documentation
* Changed how `as.data.table.Composite_table()` constructs column names. The new 
  format is colname.multiname instead of multiname.colname. This is to emulate
  the behavior of `base::merge()`
* `flip_names()` can flip names and multinames of a composite table (at the cost 
  of reordering the columns)


# tatoo 1.0.6

* Removed dplyr dependency


# tatoo 1.0.5

* Initial CRAN release.



