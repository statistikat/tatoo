#' A list of Pub_tables
#'
#' A Pub report is a list of Pub_tables. Right now this class is mostly a
#' placeholder, but in the future it will support various export methods.
#'
#' @param dat
#'
#' @return An object of class 'Pub_report'
#' @export
pub_report <- function(dat){
  res <- data.table::copy(dat)
  class(res) <- union('Pub_report', class(res))
  hammr::assert_valid(res)
  return(res)
}



#' @export
is_valid.Pub_report <- function(dat){
  res <- list()
  checkColClass <- function(x) 'Pub_table' %in% class(x)

  res$class     <- 'list' %in% class(dat)
  res$elclasses <- all(unlist(lapply(dat, checkColClass)))

  all_with_warning(res)
}



#' @export
save_txt.Pub_report <- function(
  dat,
  outfile,
  polisher = identity
){
  # Prevent data tables from wrapping
  ow <- getOption('width')
  options(width = 10000)

  # Write out
  sink(outfile)
  for(el in dat){
    cat('\n\n')
    print(polisher(el), nrows = 9999, topn = 999999)
    cat('\n\n')
  }

  # Cleanup
  options(width = ow)
  sink()
}



