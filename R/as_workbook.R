# as_workbook -------------------------------------------------------------

#' Convert a Tatoo Table Object to an Excel Workbook
#'
#' `as_workbook()` converts [`Tatoo_table`] or [`Tatoo_report`] objects directly
#' to [openxlsx] `Workbook` objects. For information about additional parameters
#' please refer to the documentation of [write_worksheet()], for which
#' `as_workbook()` is just a wrapper. Additional possible function arguments way
#' vary depending on which `Tatoo_table` you want to export.
#'
#' @param x A `Tatoo_table` or `Tatoo_report`
#' @param ... Additional arguments passed on to `write_worksheet()`
#'
#' @family xlsx exporters
#' @return `as_workbook()` returns an openxlsx `Workbook` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   Species = c("setosa", "versicolor", "virginica"),
#'   length = c(5.01, 5.94, 6.59),
#'   width = c(3.43, 2.77, 2.97)
#' )
#'
#' # Assign metadata to convert dat to a Tagged_table
#'
#' title(dat) <- "Iris excerpt"
#' footer(dat) <-  "An example based on the iris dataset"
#'
#'
#' # Convert to Workbook or save als xlsx
#'
#' wb <- as_workbook(dat)
#' save_xlsx(dat, tempfile(fileext = ".xlsx"), overwrite = TRUE)
#' }
#'
as_workbook <- function(
  x,
  ...
){
  require_openxlsx()
  UseMethod("as_workbook")
}




#' @rdname as_workbook
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#'
#' @export
as_workbook.default <- function(
  x,
  sheet = 1L,
  ...
){
  assert_that(is.scalar(sheet))
  assert_that(is.character(sheet) || is_integerish(sheet))

  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(
    x = x,
    wb = wb,
    sheet = sheet,
    ...
  )
  return(wb)
}




#' Converting a [`Tatoo_report`] will result in an openxlsx Workbook with
#' several sheets. The sheet names will be generated from the names of
#' `x` if `x` has names.
#'
#' @rdname as_workbook
#' @export
as_workbook.Tatoo_report <- function(
  x,
  ...
){
  wb <- openxlsx::createWorkbook()


  for (i in seq_along(x)){
    if (is.null(names(x))){
      sheet_name <- i
    } else {
      sheet_name <- sanitize_excel_sheet_names(names(x))[[i]]
    }

    wb <- write_worksheet(
      x = x[[i]],
      wb = wb,
      sheet = sheet_name,
      append = FALSE,
      start_row = 1L,
      ...
    )

    wb %assert_class% "Workbook"
  }

  return(wb)
}




#' @export
as_workbook.Workbook <- function(x, ...){
  x
}


# write_worksheet ---------------------------------------------------------

#' Write Data to an openxlsx Worksheet
#'
#' This function is similar to [openxlsx::writeData()] from the
#' package, but rather than just writing `data.frames`,
#' `write_worksheet()` supports specialized methods for the various
#'  [`Tatoo_table`] subclasses.
#'
#' @param x A `Tatoo_table`.
#' @param wb An [openxlsx] `Workbook` object
#' @param append `logical` Whether or not to append to an existing worksheet or
#'   create a new one
#' @param start_row A scalar integer specifying the starting row to write to.
#' @param ... Additional arguments passed on to methods for overriding the
#'   styling attributes of the `Tatoo_tables` you want to export.
#' @param named_regions `logical`. If `TRUE` (default) named regions are created
#'   in the target excel file to identify different parts of the tables
#'   (header, body, column names, etc...). These named regions can, for example,
#'   be used for applying formats. Creating named regions can be switched of
#'   as this might impact performance of the excel conversion and writing of
#'   excel files for workbooks with large numbers of tables.
#' @param named_regions_prefix `character` scalar. Prefix to write in front
#'   of all named regions created by `write_worksheet`
#'
#' @inheritParams comp_table
#' @inheritParams mash_table
#' @inheritParams stack_table
#' @inheritParams as_workbook

#'
#' @return an [openxlsx] Workbook object
#' @family xlsx exporters
#'
#' @export
write_worksheet <- function(
  x,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...,
  named_regions = TRUE,
  named_regions_prefix = NA_character_
){
  wb %assert_class% "Workbook"
  assert_that(is.scalar(sheet))
  assert_that(is.flag(append))
  assert_that(is_scalar_integerish(start_row))
  require_openxlsx()

  UseMethod("write_worksheet")
}




#' @export
write_worksheet.default <- function(
  x,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...,
  named_regions = TRUE,
  named_regions_prefix = NA_character_
){
  if (!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  dots <- list(...)
  wd_args <- intersect(names(dots), names(formals(openxlsx::writeData)))

  do.call(
    openxlsx::writeData,
    args = c(
      list(wb = wb, sheet = sheet, x = x, startRow = start_row),
      dots[wd_args]
    )
  )


  if (named_regions){
    openxlsx::createNamedRegion(
      wb,
      sheet = sheet,
      rows = seq(start_row, start_row + nrow(x)),
      cols = seq_len(ncol(x)),
      name = make_region_name(table_id(x), named_regions_prefix, "table")
    )

    openxlsx::createNamedRegion(
      wb,
      sheet = sheet,
      rows = start_row,
      cols = seq_len(ncol(x)),
      name = make_region_name(table_id(x), named_regions_prefix, "table", "colnames")
    )

    openxlsx::createNamedRegion(
      wb,
      sheet = sheet,
      rows = seq(start_row + 1, start_row + nrow(x)),
      cols = seq_len(ncol(x)),
      name = make_region_name(table_id(x), named_regions_prefix, "table", "body")
    )
  }

  wb %assert_class% "Workbook"
  return(wb)
}




#' @export
#' @rdname write_worksheet
write_worksheet.Tagged_table <- function(
  x,
  wb,
  sheet = sanitize_excel_sheet_names(attr(x, "meta")$table_id),
  append = FALSE,
  start_row = 1L,
  ...,
  print_table_id = attr(x, "meta")[[".print_table_id"]],
  named_regions = TRUE,
  named_regions_prefix = NA_character_
){
  wb %assert_class% "Workbook"
  assert_that(has_attr(x, "meta"))
  meta <- attr(x, "meta")


  wb <- wb$copy()

  if (!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  crow   <- start_row

  # Construct header
  header <- list()

  if (!is.null(meta$table_id) && !is.null(meta$title) && isTRUE(meta$.print_table_id)){
    header$title <- paste(meta$table_id, meta$title, sep = ": ")

  } else if (!is.null(meta$title)){
    header$title <- meta$title

  } else {
    header$title <- NULL
  }


  if (!identical(meta$longtitle, meta$title)){
    header$longtitle <- meta$longtitle
  }

  if (!is.null(meta$subtitle)){
    header$subtitle <- meta$subtitle
  }

  header <- unlist(header)

  # write header
  openxlsx::writeData(
    wb,
    sheet = sheet,
    header,
    rowNames = FALSE,
    colNames = FALSE,
    startRow = crow
  )


  if (named_regions && length(header) > 0){
    openxlsx::createNamedRegion(
      wb,
      sheet = sheet,
      cols  = 1L,
      rows  = seq(crow, crow + length(header) - 1L),
      name  = make_region_name(named_regions_prefix, table_id(x), "header")
    )
  }


  # write data
  crow <- crow + length(header) + 1
  ## hacky, but NextMethod did not do what i wanted when ... were passed to
  ## this function
  class(x) <- class(x)[!class(x) == "Tagged_table"]

  wb <- write_worksheet(
    x,
    wb = wb,
    sheet = sheet,
    append = TRUE,
    start_row = crow,
    ...,
    named_regions = named_regions,
    named_regions_prefix = named_regions_prefix
  )


  # Write Footer
  if (!is.null(meta$footer)){
    crow <- get_final_wb_row(wb, sheet)

    crow <- crow + 2
    openxlsx::writeData(
      wb,
      sheet = sheet,
      startRow = crow,
      meta$footer
    )

    if (named_regions){
      openxlsx::createNamedRegion(
        wb,
        sheet = sheet,
        cols  = 1L,
        rows  = seq(crow, crow + length(meta$footer) - 1L),
        name  = make_region_name(table_id(x), "footer")
      )
    }
  }

  return(wb)
}




#' @export
#' @rdname write_worksheet
write_worksheet.Composite_table <- function(
  x,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...,
  named_regions = TRUE,
  named_regions_prefix = NA_character_
){
  # Pre-condtions
  assert_that(has_attr(x, "multinames"))

  # Process arguments
  wb <- wb$copy()

  if (!append){
    openxlsx::addWorksheet(wb, sheet)
  }
  crow   <- start_row
  multinames <- attr(x, "multinames")

  assert_that(multinames %identical% sort(multinames))

  title_row     <- vector(mode = "list", length = ncol(x))
  title_counter <- 1

  for (i in seq_along(title_row)){
    title_row[[i]] <- names(multinames)[[title_counter]]

    if (i %in% multinames){
      title_counter <- title_counter + 1
    }
  }


  # Write "subtable" headings
  openxlsx::writeData(
    wb,
    sheet = sheet,
    as.data.frame(title_row),
    colNames = FALSE,
    startRow = crow
  )

  if (named_regions){
    openxlsx::createNamedRegion(
      wb,
      sheet = sheet,
      rows = crow,
      cols = seq_along(x),
      name = make_region_name(named_regions_prefix, table_id(x), "composite", "table", "multinames")
    )
  }

  crow <- crow + 1

  ## merge subtable heading cells
  for (i in seq_along(multinames)){
    merge_start <- ifelse(i == 1L, 1, multinames[[i-1]] + 1)  #nolint
    merge_end   <- multinames[[i]]
    openxlsx::mergeCells(
      wb,
      cols = c(merge_start, merge_end),
      rows = start_row,
      sheet = sheet
    )
  }

  write_worksheet(
    as.data.frame(x, multinames = FALSE),
    wb,
    sheet = sheet,
    start_row = crow,
    append = TRUE,
    named_regions = named_regions,
    named_regions_prefix = c(named_regions_prefix, "composite"),
    ...
  )

  return(wb)
}




#' @rdname write_worksheet
#' @export
write_worksheet.Mashed_table <- function(
  x,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  mash_method = attr(x, "mash_method"),
  id_vars  = attr(x, "id_vars"),
  insert_blank_row = attr(x, "insert_blank_row"),
  sep_height = attr(x, "sep_height"),
  ...,
  named_regions = TRUE,
  named_regions_prefix = NA_character_
){
  # Preconditions
    assert_that(mash_method %identical% "col" || mash_method %identical% "row")
    assert_that(is.flag(insert_blank_row))
    assert_that(is_scalar_integerish(sep_height))
    assert_that(is.null(id_vars) || is.character(id_vars))


  # Process arguments
    sep_height <- as.integer(sep_height)
    wb <- wb$copy()

    if (!append){
      openxlsx::addWorksheet(wb, sheet)
    }

    if (mash_method %identical% "col" &&
       length(names(x)) %identical% length(x)
    ){
      res <- as_Composite_table(x, meta = NULL)
    } else {
      res <- as.data.table(
        x,
        mash_method = mash_method,
        id_vars = id_vars,
        insert_blank_row = insert_blank_row
      )
    }

    assert_that(!is_Mashed_table(res))  # prevent infinite loop


  # Write data
    wb <- write_worksheet(
      res,
      wb = wb,
      sheet = sheet,
      append = TRUE,
      start_row = start_row,
      named_regions = named_regions,
      named_regions_prefix = c(named_regions_prefix, table_id(x), mash_method, "mashed"),
      ...
    )


  # Modify row heights
    row_off          <- start_row - 1
    sep_height_start <- length(x) + 2  # +2 because of header

    if (mash_method %identical% "row" && nrow(res) > length(x)){
      if (insert_blank_row){
        sel_rows <- seq(
          sep_height_start + row_off, nrow(res) + row_off,
          by = (length(x) + 1)
        )
      } else {
        sel_rows <- seq(
          sep_height_start + row_off, nrow(res) + row_off,
          by = length(x)
        )
      }

      openxlsx::setRowHeights(
        wb,
        sheet = sheet,
        rows = sel_rows,
        heights = rep(sep_height, length(sel_rows))
      )
    }

  return(wb)
}




#' @rdname write_worksheet
#' @export
write_worksheet.Stacked_table <- function(
  x,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  spacing = attr(x, "spacing"),
  ...,
  named_regions = TRUE,
  named_regions_prefix = NA_character_
){
  crow    <- start_row

  wb <- write_worksheet(
    x[[1]],
    wb = wb,
    sheet = sheet,
    start_row = crow,
    append = append,
    ...,
    named_regions = named_regions,
    named_regions_prefix = c(table_id(x), "stacked")
  )


  for (i in seq_along(x)[-1]){
    crow <- get_final_wb_row(wb, sheet)
    crow <- crow + 1 + spacing

    wb <- write_worksheet(
      x[[i]],
      wb = wb,
      sheet = sheet,
      start_row = crow,
      append = TRUE,
      named_regions_prefix = c(table_id(x), "stacked"),
      ...
    )
  }

  return(wb)
}




# save_xlsx ---------------------------------------------------------------

#' @description `save_xlsx()` is a wrapper for saving a `Tatoo_table` directly
#'   to an \file{xlsx} file.
#'
#' @template outfile
#' @template overwrite
#'
#' @return `save_xlsx()` returns the path to the saved \file{.xlsx} (invisibly).
#' @export
#' @rdname as_workbook
#'
save_xlsx <- function(
  x,
  outfile,
  overwrite = FALSE,
  ...
){
  assert_that(
    is_scalar_character(outfile),
    is.flag(overwrite)
  )
  require_openxlsx()

  UseMethod("save_xlsx")
}




#' @export
save_xlsx.default <- function(
  x,
  outfile,
  overwrite = FALSE,
  ...
){
  wb <- as_workbook(x, ...)
  openxlsx::saveWorkbook(wb, outfile, overwrite)
  invisible(outfile)
}




#' @description  `view_xlsx()` is another wrapper for viewing a `Tatoo_table`"s
#' \file{xlsx} representation in your favourite spreadsheet program (powered by
#' [openxlsx::openXL()]).
#'
#' @return `view_xlsx()` opens an external program and returns `NULL` (invisibly).
#'
#' @rdname as_workbook
#' @export
view_xlsx <- function(
  x,
  ...
){
  openxlsx::openXL(as_workbook(x, ...,  named_regions = FALSE))
  invisible()
}




make_region_name <- function(...){
  x <- list(...)
  x <- x[sapply(x, Negate(is.null))]
  x <- unlist(x)
  x <- c(x[!is.na(x)], stringi::stri_rand_strings(1, 8))
  paste(x, collapse = "_")
}
