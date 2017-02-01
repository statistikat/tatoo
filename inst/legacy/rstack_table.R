#' Format as row-stacked tabled
#'
#' @param line1 data.frame containing the first row of grouped table
#' @param line2 data.frame containing the second row of grouped table
#' @param insert_empty_line Should an empty line be inserted between grouped tables? (default FALSE for latex, FALSE for excel)
#' @param remove_ext Extension behind column names to remove a regex string (f.e. '_cv', '.x', etc.)
#' @param as_character Shoudl table cells be coerced to character? (Necessary for insert_empty_line)
#'
#' @export
rstack_table <- function(line1,
                         line2,
                         format = 'latex',
                         insert_empty_line = FALSE,
                         remove_ext = NULL,
                         as_character = TRUE,
                         format_args_line1 = list(big.mark = '~', digits = 0, format = 'f'),
                         format_args_line2 = list(digits = 2, format = 'f')){

  # Prepare input data.frames
    line1 <- as.data.frame(line1)
    line2 <- as.data.frame(line2)

    if(!is.null(remove_ext)){
      names(line1) <- gsub(remove_ext, '', names(line1))
      names(line2) <- gsub(remove_ext, '', names(line2))
    }


  # Ensure inputs are as expected
    assert_that(identical(sort(names(line1)), sort(names(line2))))
    assert_that(identical(nrow(line1), nrow(line2)))

    if(format == 'excel') format <- 'xlsx'
    assert_that(format %in% c('latex', 'excel', 'xlsx'))


    line2 <- line2[names(line1)]


  # Process function parameters
    if(insert_empty_line) {
      if(!as_character)
        warning('Columns will be coerced to character if insert_empty_line = TRUE')
      as_character <- TRUE
    }

    # Conver factors to character, to be on the safe side
    line1 <- typecast_all(line1, from = 'factor', to = 'character')
    line2 <- typecast_all(line2, from = 'factor', to = 'character')


    # Convert numbers to character (and apply a number format)
    if(as_character){
      format_line1 <- function(.x) do.call(formatC, args = c(list(x = .x),  format_args_line1))
      format_line2 <- function(.x) do.call(formatC, args = c(list(x = .x),  format_args_line2))


    # Apply number format
      sel_vars <-  names(line1)[unlist(lapply(line1, class)) == 'numeric']
      sel_vars2 <- names(line2)[unlist(lapply(line2, class)) == 'numeric']
      assert_that(identical(sel_vars, sel_vars2))

      line1[sel_vars] <- lapply(line1[sel_vars], format_line1)
      line2[sel_vars] <- lapply(line2[sel_vars], format_line2)
    }


  # Merge inputs together
    res <- foreach(i = 1:nrow(line1), .combine = rbind) %do% {
      if (format == 'latex') {
        r <- paste(line1[i,], line2[i,], sep = ' \\newline ') %>%
          t() %>%
          as.data.frame()
        names(r) <-  names(line1)
      } else {
        r <- rbind(line1[i,], line2[i,])
      }


      if (insert_empty_line && i != nrow(line1)) {
        el <- rep('', length(line1)) %>%
          t() %>%
          as.data.frame(stringsAsFactors = FALSE)
        names(el) <- names(line1)
        r <- rbind(r, el)
      }

      return(r)
    }

    class(res) <- union(paste0('Rstack_table_', format), class(res))

    return(res)
}


removetex <- function(x){
  x <- stringr::str_replace(x, '\\\\newline', '\n')
  x <- stringr::str_replace(x, '~', ' ')
}
