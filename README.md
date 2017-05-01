# tatoo

Tatoo ("Table Tools") provides functions to combine data.frames in ways that 
  require additional effort in  base R, and to add metadata (id, title, ...) 
  that can be used for printing and xlsx export. The 'Tatoo_report' class is 
  provided as a convenient helper to write several such tables to a workbook, 
  one table per worksheet. In addition, the `Tatto_report`
  class is provided as a convenient helper to write several such tables to a 
  workbook, one table per worksheet.
  
## Example

```R
tag_table(head(cars), tt_meta(title = "Data about cars"))
mash_table(head = head(cars), tail = tail(cars))
mash_table(head = head(cars), tail = tail(cars), mash_method = "col")
comp_table(head = head(cars), tail = tail(cars))
stack_table(head = head(cars), tail = tail(cars))
```
