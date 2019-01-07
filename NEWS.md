# tatoo 1.1.0.9000

* removed **purrr** and **rlang** dependencies


# tatoo 1.1.0

* Add Named Regions for table parts Excel Sheets to make formatting easier
* Added `walk_regions()` and `map_regions()` to mainuplate cells in named
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
* `save_xlsx()` now returns the path to the saved workbook, raher than the
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
  the behaviour of `base::merge()`
* `flip_names()` can flip names and multinames of a composite table (at the cost 
  of reordering the columns)


# tatoo 1.0.6

* Removed dplyr dependency


# tatoo 1.0.5

* Initial CRAN release.



