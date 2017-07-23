#* @testfile test_save_xlsx

# as_workbook -------------------------------------------------------------

#' Convert a Tatoo table object to an Excel workbook
#'
#' This function converts Tatoo_table objects directly to [openxlsx] Workbook
#' objects. For information about additional parameters please refer to the
#' documentation of `write_worksheet()`, for which `as_workbook()`
#' is just a wrapper. Additional possible function arguments way vary depending
#' on which Tatoo_table you want to export.
#'
#' @param dat a [Tatoo_table] or [Tatoo_report]
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param ... passed on to [write_worksheet()]
#'
#' @md
#' @family xlsx exporters
#' @return an openxlsx [openxlsx] Workbook object (invisibly for `save.xlsx`)
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
#' title(dat) <- 'Iris excerpt'
#' footer(dat) <-  'An example based on the iris dataset'
#'
#'
#' # Convert to Workbook or save als xlsx
#'
#' wb <- as_workbook(dat)
#' save_xlsx(dat, 'iris.xlsx', overwrite = TRUE)
#' }
#'
as_workbook <- function(
  dat,
  sheet = 1L,
  ...
){
  assert_that(requireNamespace("openxlsx"))
  assert_that(is.scalar(sheet))

  UseMethod('as_workbook')
}




#' @rdname as_workbook
#' @export
as_workbook.default <- function(
  dat,
  sheet = 1L,
  ...
){
  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(
    dat = dat,
    wb = wb,
    sheet = sheet,
    ...
  )
  return(wb)
}




#' Converting a \code{Tatoo_report} will result in a \code{Workbook} with
#' several sheets. The sheet names will be generated from the names of
#' \code{dat} (if \code{dat} has names).
#'
#' @rdname as_workbook
#' @export
as_workbook.Tatoo_report <- function(dat, ...){
  wb <- openxlsx::createWorkbook()

  for(i in seq_along(dat)){
    if(is.null(names(dat))){
      sheet_name <- i
    } else {
      sheet_name <- sanitize_excel_sheet_names(names(dat))[[i]]
    }

    wb <- write_worksheet(
      dat = dat[[i]],
      wb = wb,
      sheet = sheet_name,
      append = FALSE,
      start_row = 1L,
      ...
    )

    wb %assert_class% 'Workbook'
  }

  return(wb)
}



# write_worksheet ---------------------------------------------------------

#' Write data to an openxlsx Worksheet
#'
#' This function is similar to [openxlsx::writeData()] from the
#' [openxlsx] package, but rather than just writing data.frames,
#' `write_worksheet`` supports specialized methods for the various
#'  [Tatoo_table] subclasses.
#'
#' @param dat A [Tatoo_table].
#' @param wb A [openxlsx] Workbook object
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param append Logical. Whether or not to append to an existing worksheet or
#'   create a new one
#' @param start_row A scalar specifying the starting row to write to.
#' @param ... additional options that can be used override the styling
#'   attributes of the [Tatoo_table] you want to export.
#'
#' @inheritParams comp_table
#' @inheritParams mash_table
#' @inheritParams stack_table
#'
#' @md
#' @return an [openxlsx] Workbook object
#' @family xlsx exporters
#'
#' @export
write_worksheet <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  wb %assert_class% 'Workbook'
  assert_that(is.scalar(sheet))
  assert_that(is.flag(append))
  assert_that(is.number(start_row))
  assert_that(requireNamespace("openxlsx"))

  UseMethod('write_worksheet')
}



#' @export
write_worksheet.default <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  openxlsx::writeData(
    wb = wb,
    sheet = sheet,
    x = dat,
    startRow = start_row)

  wb %assert_class% 'Workbook'
  return(wb)
}




#' @export
#' @rdname write_worksheet
write_worksheet.Tagged_table <- function(
  dat,
  wb,
  sheet = sanitize_excel_sheet_names(attr(dat, 'meta')$table_id),
  append = FALSE,
  start_row = 1L,
  ...
){
  wb %assert_class% 'Workbook'
  assert_that(has_attr(dat, 'meta'))
  meta <- attr(dat, 'meta')

  wb <- wb$copy()

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  crow   <- start_row

  # Construct header
  header <- list()

  if (!is.null(meta$table_id) && !is.null(meta$title)){
    header$title <- paste(meta$table_id, meta$title, sep = ': ')

  } else if (!is.null(meta$title)){
    header$title <- meta$title

  } else {
    header$title <- NULL
  }


  if(!identical(meta$longtitle, meta$title)){
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


  # write data
  crow <- crow + length(header) + 1
  ## hacky, but NextMethod did not do what i wanted when ... were passed to
  ## this function
  class(dat) <- class(dat)[!class(dat) == 'Tagged_table']

  wb <- write_worksheet(
    dat,
    wb = wb,
    sheet = sheet,
    append = TRUE,
    start_row = crow,
    ...
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
  }


  return(wb)
}




#' @export
#' @rdname write_worksheet
write_worksheet.Composite_table <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  # Pre-condtions
  assert_that(has_attr(dat, 'multinames'))

  # Process arguments
  wb <- wb$copy()

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }
  crow   <- start_row
  multinames <- attr(dat, 'multinames')

  assert_that(multinames %identical% sort(multinames))

  title_row     <- vector(mode = 'list', length = ncol(dat))
  title_counter <- 1

  for(i in seq_along(title_row)){
    title_row[[i]] <- names(multinames)[[title_counter]]

    if(i %in% multinames){
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

  crow <- crow + 1

  ## merge subtable heading cells
  for(i in seq_along(multinames)){
    merge_start <- ifelse(i == 1L, 1, multinames[[i-1]] + 1)
    merge_end   <- multinames[[i]]
    openxlsx::mergeCells(
      wb,
      cols = c(merge_start, merge_end),
      rows = start_row,
      sheet = sheet
    )
  }


  # Write data
  openxlsx::writeData(
    wb,
    sheet = sheet,
    startRow = crow,
    as.data.frame(dat, multinames = FALSE),
    colNames = TRUE
  )

  return(wb)
}







#' @rdname write_worksheet
#' @export
write_worksheet.Mashed_table <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  mash_method = attr(dat, 'mash_method'),
  id_vars  = attr(dat, 'id_vars'),
  insert_blank_row = attr(dat, 'insert_blank_row'),
  sep_height = attr(dat, 'sep_height'),
  ...
){
  # Preconditions
    assert_that(mash_method %identical% 'col' || mash_method %identical% 'row')
    assert_that(is.flag(insert_blank_row))
    assert_that(rlang::is_scalar_integerish(sep_height))
    assert_that(is.null(id_vars) || is.character(id_vars))


  # Process arguments
    sep_height <- as.integer(sep_height)
    wb <- wb$copy()

    if(!append){
      openxlsx::addWorksheet(wb, sheet)
    }

    if(mash_method %identical% 'col' &&
       length(names(dat)) %identical% length(dat)
    ){
      res <- as_Composite_table(dat, meta = NULL)
    } else {
      res <- as.data.table(
        dat,
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
      start_row = start_row
    )

  # Modify row heights
    row_off          <- start_row - 1
    sep_height_start <- length(dat) + 2  # +2 because of header

    if(mash_method %identical% 'row' && nrow(res) > length(dat)){
      if(insert_blank_row){
        sel_rows <- seq(
          sep_height_start + row_off, nrow(res) + row_off,
          by = (length(dat) + 1)
        )
      } else {
        sel_rows <- seq(
          sep_height_start + row_off, nrow(res) + row_off,
          by = length(dat)
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
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  spacing = attr(dat, 'spacing'),
  ...
){
  crow    <- start_row

  wb <- write_worksheet(
    dat[[1]],
    wb = wb,
    sheet = sheet,
    start_row = crow,
    append = append,
    ...
  )


  for(i in seq_along(dat)[-1]){
    crow <- get_final_wb_row(wb, sheet)
    crow <- crow + 1 + spacing

    wb <- write_worksheet(
      dat[[i]],
      wb = wb,
      sheet = sheet,
      start_row = crow,
      append = TRUE,
      ...)
  }

  return(wb)
}
