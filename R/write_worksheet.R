#* @testfile test_save_xlsx

#' Write data to an openxlsx Worksheet
#'
#' This function is similar to \code{\link[openxlsx]{writeData}}, but
#' rather than writing \code{data.frames}, \code{write_data} supports methods
#' for the various \code{tatoo} classes.
#'
#' @param dat A tatoo table object, \code{Comp_table}, \code{Meta_table},
#'   \code{Mash_table} or \code{Stack_table}.
#' @param wb A \code{\link[openxlsx]{openxlsx}} Workbook object
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param append Logical. Whether or not to append to an exisiting worksheet or
#'   create a new one
#' @param start_row A scalar specifiying the starting row to write to.
#' @param ... passed onto methods
#'
#' @return
#' @export
#'
#' @examples
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
write_worksheet.Meta_table <- function(
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
    class(dat) <- class(dat)[!class(dat) == 'Meta_table']

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
write_worksheet.Comp_table <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  # Pre-condtions
    assert_that(has_attr(dat, 'titles'))

  # Process arguments
    wb <- wb$copy()

    if(!append){
      openxlsx::addWorksheet(wb, sheet)
    }
    crow   <- start_row
    titles <- attr(dat, 'titles')

    assert_that(titles %identical% sort(titles))

    title_row     <- vector(mode = 'list', length = ncol(dat))
    title_counter <- 1

    for(i in seq_along(title_row)){
      title_row[[i]] <- names(titles)[[title_counter]]

      if(i %in% titles){
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
    for(i in seq_along(titles)){
      merge_start <- ifelse(i == 1L, 1, titles[[i-1]] + 1)
      merge_end   <- titles[[i]]
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




#' @export
write_worksheet.Mash_table <- function(
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




#' @export
write_worksheet.Stack_table <- function(
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
