#* @testfile test_save_xlsx

#' Title
#'
#' @param dat
#' @param wb
#' @param sheet
#' @param passed on to \code{openxlsx::writeData}
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



#' Write worksheet
#'
#' @param dat
#' @param wb
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
write_worksheet.Meta_table <- function(
  dat,
  wb,
  sheet = sanitize_excel_sheet_names(attr(dat, 'meta')$table_id),
  append = FALSE,
  start_row = 1L,
  ...
){
  wb %assert_class% 'Workbook'
  assert_that(is.flag(append))
  assert_that(is.number(start_row))
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
      start_row = crow
    )


  # Write Footer
    if (!is.null(meta$footer)){

      crow <- openxlsx::readWorkbook(
        xlsxFile = wb,
        sheet = sheet,
        colNames = FALSE,
        skipEmptyRows = FALSE
      ) %>%
        nrow()

      crow <- crow + 3
      openxlsx::writeData(
        wb,
        sheet = sheet,
        startRow = crow,
        meta$footer
      )
    }


  return(wb)
}


#' Title
#'
#' @param dat
#' @param wb
#' @param sheet
#' @param append
#' @param start_row
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

    if(mash_method %identical% 'row'){
      if(insert_blank_row){
        sel_rows <- seq(
          sep_height_start + row_off, nrow(res) + row_off,
          by = (length(dat) + 1)
        )
      } else {
        sel_rows <- seq(
          sep_height_start + row_off, nrow(res) + row_off, by = length(dat)
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



write_worksheet.Stack_table <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L
){
  for(table in dat){
    start_row <- nrow(openxlsx::readWorkbook(
      wb,
      sheet = sheet,
      colNames = FALSE,
      skipEmptyRows = FALSE)
    )

    start_row <- start_row + 5

    wb <- write_worksheet(
      table,
      wb = wb,
      sheet = sheet,
      start_row = start_row,
      append = TRUE)
  }

  return(wb)
}

# Utils -------------------------------------------------------------------

#' Sanitze excel sheet names
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sanitize_excel_sheet_names <- function(x){
  invalid_chars_regex <- "\\[|\\]|\\*|\\?|:|\\/|\\\\"
  res <- stringi::stri_replace_all_regex(x, invalid_chars_regex, '_')
  res <- stringi::stri_sub(res, 1, 31)

  for(el in unique(res)){
    suffix <- cumsum(duplicated(res[res == el]))

    if(length(res[res == el]) > 1L){
      res[res == el] <- paste0(
        strtrim(res[res == el], 31 - max(nchar(suffix))),
        suffix)
    }
  }

  return(res)
}
