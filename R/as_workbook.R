#* @testfile test_save_xlsx

#' Convert a Tatto table object to an Excel workbook
#'
#' This function converts \code{\link{Tatoo_table}} obects directly to
#' \code{openxlsx} \code{Workbook} objects. For information about additional
#' parameters please refer to the documentation of
#' \code{\link{write_worksheet}}, for which \code{as_workbook} is just a wraper.
#'
#' @param dat Usually a \code{Tatoo_table} or \code{Tatoo_report}.
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param ... passed on to \code{\link{write_worksheet}}
#'
#' @return an openxlsx \code{\link[openxlsx]{Workbook}} object
#' @export
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
#' This function is similar to \code{\link[openxlsx]{writeData}} from the
#' \code{openxlsx} package, but rather than just writing \code{data.frames},
#' \code{write_worksheet} supports specialised methods for the various
#' \code{\link{Tatoo_table}} subclasses.
#'
#' @param dat A \code{\link{Tatoo_table}}.
#' @param wb A \code{\link[openxlsx]{openxlsx}} Workbook object
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param append Logical. Whether or not to append to an exisiting worksheet or
#'   create a new one
#' @param start_row A scalar specifiying the starting row to write to.
#' @param mash_method for \code{Mashed_tables}: mash by \code{"row"} or by
#'   \code{"col"}
#' @param insert_blank_row for \code{Mashed_tables}: logical. insert a blank row
#'   between mash-pairs? (only available when mashing by row)
#' @param sep_height for \code{Mashed_tables}: If \code{insert_blank_row} is
#'   \code{TRUE}, height of the sepparating blank line, else height of the first
#'   line of the mash pair (starting with the second mash pair)
#' @param ... ignored
#'
#' @return an \code{openxlsx Workbook}
#'
#' @export
#'
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

  header$title <- sprintf('%s: %s', meta$table_id, meta$title)

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
    dat,
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
  mash_method = 'row',
  insert_blank_row = TRUE,
  sep_height = 30,
  ...
){
  # Preconditions
  assert_that(is.scalar(mash_method))
  assert_that(is.flag(insert_blank_row))
  assert_that(is.number(sep_height))


  # Process arguments
  wb <- wb$copy()

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  res <- as.data.table(
    dat,
    mash_method = mash_method,
    insert_blank_row = insert_blank_row
  )

  # Write data
  openxlsx::writeData(
    wb,
    sheet = sheet,
    res,
    startRow = start_row
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
  ...
){
  crow    <- start_row
  spacing <- attr(dat, 'spacing')


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
