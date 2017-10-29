# tatoo 1.0.8.9000

* Convert `Tatoo_table` objects to Latex code or save them directly to `.pdf`
  with `as_latex()` and `save_pdf`.
* Added `open_file()` utility to open files with external programs.

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



