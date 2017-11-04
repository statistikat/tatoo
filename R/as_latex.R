#* @testfile manual_tests/test_output_formats

#' Convert an \R Object to Latex code
#'
#'
#'
#' @section Latex Packages:
#' Code created with `as_latex` assumes that the following Latex packages are
#' loaded:
#'
#' ```
#' \usepackage{booktabs}
#' \usepackage{longtable}
#' \usepackage{threeparttablex}
#'```
#'
#' @param x
#' @param ... passed on to methods
#' @param kable_options
#'
#' @return
#' @export
#'
#' @examples
as_latex <- function(x, ..., kable_options = default_kable_options){
  require_knitr()
  UseMethod("as_latex")
}




as_latex.default <- function(...){
  "not supported"
}




#' Title
#'
#' @param x
#' @inheritParams tag_table
#' @param ...
#' @param kable_options
#'
#' @return
#' @export
#'
#' @examples
as_latex.Tagged_table <- function(
  x,
  ...,
  kable_options = default_kable_options
){
  meta  <- attr(x, 'meta')

  if (!is.null(meta)){
    caption <- paste(
      c(make_tag_table_print_title(meta)),
      collapse = " "
    )
    ko <- c(list(caption = caption), kable_options)
  } else {
    ko <- kable_options
  }

  res <- NextMethod(as_latex, x, ..., kable_options = ko)

  res <- gsub(
    "\\\\bottomrule.\\\\endlastfoot",  #nolint
    "\\\\bottomrule\n\\\\insertTableNotes\n\\\\endlastfoot",  #nolint
    res
  )

  res <- sprintf(
  "\\begin{ThreePartTable}
    \\begin{TableNotes}
      \\item %s
    \\end{TableNotes}
      %s
  \\end{ThreePartTable}",
  meta$footer,
  res
  )

  res
}




#' @inheritParams tag_table
#' @inheritParams tag_table as_latex
#' @export
#'
as_latex.Composite_table <- function(
  x,
  ...,
  kable_options = default_kable_options
){
  header <- multinames_to_colspans(multinames(x))

  res <- x %>%
    as.data.table(
      id_vars = id_vars,
      multinames = FALSE
    ) %>%
    {do.call(knitr::kable, c(list(.), kable_options))} %>%  #nolint
    kableExtra::kable_styling(latex_options = c("repeat_header")) %>%
    kableExtra::add_header_above(header = header)
}




as_latex.Tatoo_report <- function(x, ...){
  x %>%
    lapply(as_latex) %>%
    unlist() %>%
    paste(sep = "\n", collapse = "\n")
}




#' Title
#'
#' @param x
#' @inheritParams mash_table
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_latex.Mashed_table <- function(
  x,
  mash_method = attr(x, 'mash_method'),
  id_vars  = attr(x, 'id_vars'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  sep_height = attr(x, 'sep_height'),
  ...,
  kable_options = default_kable_options
){
  assert_that(is.flag(insert_blank_row))
  sep_height <- sep_height - 12 * !insert_blank_row


  if (identical(mash_method, "row")){
    linesep <- c(
      rep('', length(x) - 1 + insert_blank_row),
      sprintf("\\addlinespace[%spt]", sep_height)
    )
  } else {
    linesep <- ""
  }

  if(
    identical(mash_method, "col") &&
    identical(length(names(x)), length(x))
  ){
    return(as_latex(as_Composite_table(x)))
  } else {
    header <- NULL
  }

  kable_options <- c(list(linesep = linesep), kable_options)

  res <- x %>%
    as.data.table(
      id_vars = id_vars,
      insert_blank_row = insert_blank_row,
      suffixes = NULL
    ) %>%
    {do.call(knitr::kable, c(list(.), kable_options))} %>%  #nolint
    kableExtra::kable_styling(latex_options = c("repeat_header"))

  if (!is.null(header)){
    res <- kableExtra::add_header_above(res, header)
  }

  res
}




as_latex.data.frame <- function(
  x,
  ...,
  kable_options = default_kable_options
){
  do.call(
    knitr::kable,
    args = c(list(x), kable_options)
  ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"))
}




default_kable_options <- list(
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE
)




#' `save_pdf()` is a convenience wrapper around [as_latex()] for directly saving
#' an \R object to \file{.pdf}.
#'
#' @rdname as_latex
#' @param x
#' @param outfile
#' @oaram ... passed on to methods
#' @param overwrite
#' @param template
#' @param papersize `character` scalar. Passed on to the latex command
#'   `\\geometry` from the 'geometry' package. Valid values are: `a0paper,
#'   a1paper, a2paper, a3paper, a4paper, a5paper, a6paper, b0paper, b1paper,
#'   b2paper, b3paper, b4paper, b5paper, b6paper, c0paper, c1paper, c2paper,
#'   c3paper, c4paper, c5paper, c6paper, b0j, b1j, b2j, b3j, b4j, b5j, b6j,
#'   ansiapaper, ansibpaper, ansicpaper, ansidpaper, ansiepaper, letterpaper,
#'   executivepaper, legalpaper`
#' @param orientation `character` scalar. Passed on to the latex command
#'   `\\geometry` from the 'geometry' package. Valid values are:
#'   `portrait`, `landscape`
#' @param keep_source
#'
#' @return
#' @export
#'
#' @examples
save_pdf <- function(
  x,
  outfile = paste0(paste(substitute(x), collapse = "_"), ".pdf"),
  ...,
  overwrite = FALSE,
  papersize = "a4paper",
  orientation = "portrait",
  keep_source = FALSE,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo")
){
  UseMethod("save_pdf")
}




#' @export
#' @rdname save_pdf
save_pdf.default <- function(
  x,
  outfile = paste0(make.names(paste(substitute(x), collapse = "_")), ".pdf"),
  ...,
  overwrite = FALSE,
  papersize = "a4paper",
  orientation = "portrait",
  keep_source = FALSE,
  kable_options = default_kable_options,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo")
){
  temp <- readLines(template)

  if (file.exists(outfile)){
    if (overwrite) unlink(outfile)
    else stop(sprintf("'%s' already exists.", outfile))
  }


  tex <- as_latex(x, ..., kable_options = kable_options)
  tex <- gsub("{TABLE}", tex, temp, fixed = TRUE)
  tex <- gsub("{PAPERSIZE}", papersize, tex, fixed = TRUE)
  tex <- gsub("{ORIENTATION}", orientation, tex, fixed = TRUE)
  tex <- paste(tex, collapse = "\n")

  tf <- paste0(tempfile(), c(".tex", ".pdf"))
  writeLines(tex, tf[[1]])

  withr::with_dir(
    dirname(tf[[1]]),
    tools::texi2pdf(tf[[1]])
  )

  assert_that(file.copy(from = tf[[2]], to = outfile, overwrite = overwrite))

  if (keep_source){
    file.copy(tf[[1]], paste0(outfile, ".tex"), overwrite = TRUE)
  }

  invisible(outfile)
}




# utils -------------------------------------------------------------------

#' Convert multinames to colspans
#'
#' @param x a [Composite_table] [multinames] attribute.
#' @return A named character vector of colspans (for [kableExtra::add_header_above()])
#' @export
#'
multinames_to_colspans <- function(x){
  header <- diff(c(0, x))
  names(header)[names(header) == ""] <- " "
  header
}
