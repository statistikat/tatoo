#' @export
save_txt <- function(dat, outfile, ...){
  UseMethod('save_txt')
}


#' @export
save_txt.Tatoo_report <- function(
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
