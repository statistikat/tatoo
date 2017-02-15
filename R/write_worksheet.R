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
  openxlsx::writeData(wb = wb, sheet = sheet, x = dat, startRow = start_row)
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
write_worksheet.Pub_table <- function(
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

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  crow   <- start_row

  # Construct header
    header <- list()

    header$title <- sprintf('%s: %s', meta$table_id, meta$title)

    if(meta$longtitle != meta$title){
      header$longtitle <- meta$longtitle
    }

    if (!is.null(meta$subtitle)){
      header$subtitle <- meta$subtitle
    }

    header <- t(as.data.frame(header))

    openxlsx::writeData(
      wb,
      sheet = sheet,
      header,
      rowNames = FALSE,
      colNames = FALSE
    )

    crow <- crow + nrow(header) + 1
    class(dat) <- class(dat)[!class(dat) == 'Pub_table']

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
      crow <- openxlsx::readWorkbook(
        wb,
        sheet = sheet,
        colNames = FALSE,
        skipEmptyRows = FALSE
      ) %>%
        nrow()

      crow <- crow + 3
      openxlsx::writeData(wb, sheet = sheet, startRow = crow, meta$footer)
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

  # Write super-headings
  openxlsx::writeData(
    wb,
    sheet = sheet,
    as.data.frame(title_row),
    colNames = FALSE,
    startRow = crow
  )

  crow <- crow + 1


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
  assert_that(is.scalar(mash_method))
  assert_that(is.flag(insert_blank_row))
  assert_that(is.number(sep_height))

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  res <- as.data.table(
    dat,
    mash_method = mash_method,
    insert_blank_row = insert_blank_row
  )

  openxlsx::writeData(
    wb,
    sheet = sheet,
    res,
    startRow = start_row
  )


  row_off          <- start_row - 1
  sep_height_start <- length(dat) + 2  # +2 accounts for header, and that excel cell indices start with 0

  # Modify row heights
  # '4' is the empty row before the table + table heading + first pair of rows
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
