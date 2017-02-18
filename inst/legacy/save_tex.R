
mash_cols_tex <- function(dat) {
  foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
    r <- paste(dat[[1]][i, ], dat[[2]][i, ], sep = ' ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(dat[[1]])

    return(r)
  }
}


mash_rows_tex <- function(dat, insert_blank_row) {
  dat %assert_class% 'Mash_table'

  empty_row <- rep('', length(dat[[1]])) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    data.table::setnames(names(dat[[2]]))

  res <- foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
    r <- paste(dat[[1]][i, ], dat[[2]][i, ], sep = ' \\newline ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(dat[[1]])


    if (insert_blank_row && i != nrow(dat[[1]])) {
      r <- rbind(r, empty_row)
    }

    return(r)
  }
}



#' Title
#'
#' @param dat
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print_tex <- function(dat, ...){
  UseMethod('print_tex')
}


#' print a Mash_table as latex
#'
#' Stacked rows are sepparated by \code{\\newline}, therefor this only works
#' correctly for columns that have an 'X' column type (see documentation of
#' the tabularx latex package). If you want each stack element in a proper
#' tabular row, use \code{xtable::xtable(as.data.frame(dat))} instead.
#' By default this uses the \code{tabularx} and \code{booktabs} latex packages.
#' Booktabs can be switched of, but the tabularx package is hardcoded into the
#' xtable settings right now as some of the formatting magic depends on it
#'
#' @param dat an input stack table
#' @param stack Stack by by \code{row} or \code{col}
#' @param insert_blank_row insert additional empty row after each row of the
#'        tabular environment. \renewcommand{\arraystretch}{1.5}
#' @param .align the [pos] argument of the tabular environment. Passed on
#'        to \code{\link{xtable}}. Please note that \code{\\newline} only
#'        works in \code{X} columns. If you use another format the 'stacking'
#'        will not work correclty.
#' @param .caption passed on to \code{\link{xtable}}
#' @param .include.rownames passed on to \code{\link{print.xtable}}
#' @param .floating passed on to \code{\link{print.xtable}}
#' @param .tabular.environment passed on to \code{\link{print.xtable}}
#' @param .booktabs passed on to \code{\link{print.xtable}}
#' @param .sanitize.text.function passed on to \code{\link{print.xtable}}
#' @param .width passed on to \code{\link{print.xtable}}
#' @param ... Additoinal arguments passed on to \code{\link{print.xtable}}
#'
#' @return
#' @export
#'
#' @examples
print_tex.Mash_table <- function(dat,
                                 mash_method = 'row',
                                 insert_blank_row = (mash_method == 'row'),
                                 .align = paste0('lX',
                                                 paste(rep('X',
                                                           ncol(dat[[1]]) - 1),
                                                       collapse = '')),
                                 .include.rownames=FALSE,
                                 .floating = FALSE,
                                 .booktabs = TRUE,
                                 .sanitize.text.function = identity,
                                 .width = '\\textwidth',
                                 .caption = NULL,
                                 ...){

  # Preconditions
  mash_method %assert_class% 'character'
  insert_blank_row %assert_class% 'logical'
  assert_that(is.scalar(mash_method))
  assert_that(is.scalar(insert_blank_row))


  # Stacking
  res <- switch(mash_method,
                'row' = mash_rows_tex(dat, insert_blank_row = FALSE),
                'col' = mash_cols_tex(dat))

  # format latex
  xtable::xtable(
    res,
    align = .align,
    caption = .caption
  ) %>%
    print(
      include.rownames = .include.rownames,
      tabular.environment = 'tabularx',
      booktabs = .booktabs,
      sanitize.text.function = .sanitize.text.function,
      width = .width,
      ...)
}





