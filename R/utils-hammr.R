# utility functions copy and pasted from my personal utility function package.
# That package is not yet production ready, and will be added as a dependency
# once it is.


#awkward, but necessary to preven R CMD check warning
`%identical%` <- function(x, y) identical(x, y)




all_are_distinct <- function(x, empty_value = FALSE, silent = FALSE){
  assert_that(length(empty_value) <= 1)


  if (length(x) <= 1L){
    if (identical(length(x), 1L)){
      res <- TRUE
      if (!silent) warning("`x` consists of only one element")


    } else if (identical(length(x), 0L)){
      res <- empty_value
      if (!silent){
        if (is.null(x)){
          warning("`x` is NULL")
        } else {
          warning("`x` is an empty vector")
        }
      }
    }


  } else {
    res <- identical(length(unique(x)), length(x))
  }

  assert_that(
    identical(res, TRUE) ||
      identical(res, FALSE) ||
      identical(res, empty_value)
  )

  return(res)
}




# assert valid ------------------------------------------------------------

is_valid <- function(x, ...) {
  UseMethod("is_valid")
  print("test")
}




assert_valid <- function(x, ...){
  v <- is_valid(x)

  if (v){
    return(TRUE)
  } else {
    stop(assert_valid_error(x))
  }
}




assert_valid_error  <- function(obj) {
  msg <- sprintf(
    "A validity check failed for object of class: %s.",
    paste(class(obj), collapse = ", ")
  )

  condition(c("assert_valid_error", "error"),
            message = msg)
}




# assertions --------------------------------------------------------------

#' Check if object is of a certain class
#'
#' These functions are designed to be used in combination with the assertthat
#' package
#'
#' `is_class returns()` `TRUE`/`FALSE`. It comes with a on_failure function and
#' is designed to be used in conjunction with the assertthat package.
#' `assert_class()` and its infix version %assert_class% fail with an error message
#'
#' @param dat any R object
#' @param class the class to be checked for
#'
#' @return `is_class()` returns `TRUE`/`FALSE`, `assert_class()` returns `TRUE`
#'   or fails with an error message.
#' @noMd
is_class <- function(dat, class){
  inherits(dat, class)
}




on_failure(is_class) <- function(call, env){
  class <- env$class
  paste("Requires an object of class", class, "as input")
}




#' @rdname is_class
assert_class <- function(dat, class){
  assert_that(is_class(dat = dat, class = class))
}




#' @rdname is_class
`%assert_class%` <- function(dat, class){
  assert_class(dat = dat, class = class)
}




#' Check if any of the classes of the object match a certain string
#'
#' @param dat the object
#' @param choices  the class to be checked for
#'
#' @return True if any of the object classes are the desired class
#' @noMd
is_any_class <- function(dat, choices){
  any(choices %in% class(dat))
}




on_failure(is_any_class) <- function(call, env){
  choices <- paste(eval(call$choices), collapse = ", ")
  paste("Input must be an object of any of the following classes:", choices)
}




# Is col classes ----------------------------------------------------------

#' Check for column classes
#'
#' Compares the column classes of a data.frame with
#'
#' @param dat a data.frame or list
#' @param classes a list of column classes. Its names must match
#'        the names of dat exactly (see example)
#' @param method if \code{all}, ensure that all columns named in \code{classes} are present in \code{dat},
#'         if \code{any}, ensure that any of the  columns named in \code{classes} are present in \code{dat},
#'         if \code{identical}, ensure that the names of dat and classes are identical
#' @noMd
is_col_classes <- function(dat, classes, method = "identical"){
  classes %assert_class% "list"
  assert_that(
    length(classes) > 0,
    length(names(classes)) %identical% length(classes),
    is.scalar(method),
    method %in% c("all", "any", "identical")
  )

  dat <- as.list(dat)

  if (method %identical% "all"){
    assert_that(all(names(classes) %in% names(dat)))
  } else if (method %identical% "any"){
    assert_that(any(names(classes) %in% names(dat)))
    classes <- classes[names(classes) %in% names(dat)]
  } else if (method %identical% "identical"){
    assert_that(identical(names(classes), names(dat)))
  } else{
    stop("method must be 'all', 'any' or 'identical'")
  }

  res <- rep(FALSE, length(names(classes)))
  names(res) <- names(classes)

  for (i in names(classes)){
    res[i] <- i %in% names(dat) && classes[[i]] == class(dat[[i]])
  }

  all_with_warning(res)
}




assertthat::on_failure(is_col_classes) <- function(call, env){
  dat     <- eval(call$dat)
  classes <- eval(call$classes)

  present <- names(classes)[names(classes) %in% names(dat)]
  missing <- names(classes)[!names(classes) %in% names(dat)]
  wrong   <- character()

  for (i in present){
    col    <- i
    is     <- class(dat[[i]])
    should <- classes[[i]]

    if (any(is != should)){
      is_str     <- paste(is, collapse = ", ")
      should_str <- paste(should, collapse = ", ")

      wrong <- paste0(wrong, col, " (", is_str, "->", should_str, "), ")
    }
  }

  missing <- paste(missing, collapse = ", ")

  msg <- character()

  if (length(missing) > 0){
    msg <- paste0("Missing from dat: ", missing, ".\n")
  }

  if (length(wrong) > 0){
    wrong <- substr(wrong, 1, crayon::col_nchar(wrong) - 2)
    msg <- paste0(msg, "Wrong classes: ", wrong)
  }

  return(msg)
}




cfun <- function(x){
  msg <- paste("Input must be any of `numeric`, `integer`, `factor`",
               "`character`, `POSIXct`, `Date`, but is", x)

  res <- switch(x,
                "logical"   = as.logical,
                "integer"   = as.integer2,
                "factor"    = as.factor,
                "numeric"   = as.numeric2,
                "character" = as.character,
                "POSIXct"   = as.POSIXct,
                "Date"      = as.Date,
                stop(msg)
  )
  return(res)
}




as.numeric2   <- function(x) as.numeric(as.character(x))




as.integer2   <- function(x) as.integer(as.character(x))




# Misc utils --------------------------------------------------------------

all_with_warning <- function(dat){
  dat <- as.list(dat)
  dat %assert_class% "list"
  assert_that(unlist(unique(lapply(dat, class))) %identical% "logical")

  datl <- as.logical(dat)
  if (any(is.na(datl))){
    warning("Treating `NA`s as `FALSE`")
  }

  dat[which(is.na(datl))] <- FALSE
  datl[is.na(datl)]       <- FALSE

  if (all(datl)){
    return(TRUE)
  } else {
    failed      <- dat[as.logical(lapply(dat, identical, FALSE))]
    warn        <- paste(
      "`FALSE`, but should be `TRUE`:\n",
      paste(names(failed), collapse = ", ")
    )
    warning(warn)
    return(FALSE)
  }
}




df_round <- function(dat, digits = 0){
  assert_that(is.number(digits))
  numcols <- names(dat)[unlist(lapply(dat, is.numeric))]

  for (i in numcols){
    dat[[i]] <- round(dat[[i]], digits = digits)
  }

  return(dat)
}




#' Typecast all columns of a data.frame of a specific type
#'
#' Bulk convert columns of a data.frame that share a certain class to a different
#' class. Use with care, will introduce NAs for some conversion attempts
#'
#' @param dat a data.frame
#' @param from column type to cast
#' @param to target column type
#'
#' @return a data frame with all columns of class from converted to class to
#'
#' @noMd
df_typecast_all <- function(dat, from = "factor", to = "character"){
  dat   <- as.data.frame(dat)
  tofun <- cfun(to)

  vars <- names(dat)[unlist(lapply(dat, class) == from)]

  for (i in vars){
    dat[[i]] <- tofun(dat[[i]])
  }

  return(dat)
}




#' Remove linebreaks and multiple spaces from string
#'
#' @param x a character vector.
#'
#' @return a character vector without linebreaks
#' @noMd
str_nobreak <- function(x){
  y <- gsub("\r?\n|\r", " ", x)
  gsub("[ ]{2,}", " ", y)
}




condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}




#' Rearrange vector based on priorities
#'
#' Shoves elements of a character vector to the front or back.
#' Throws a warning if any elements of `high` or `low` are not present in `x`.
#'
#' @param x a character vector
#' @param high elements to be put to the front
#' @param low elements to be put to the back
#'
#' @return a reordered vector
#' @noMd
vec_prioritise <- function(x, high = NULL, low = NULL){
  low_not_x  <- low[!low %in% x]
  high_not_x <- high[!high %in% x]

  if (!all(low  %in% x)) {
    warning(
      "Not all `low` are present in `x`: ",
      paste(low_not_x, collapse = " "))
  }
  if (!all(high %in% x)){
    warning(
      "Not all `high` are present in `x`: ",
      paste(high_not_x, collapse = " "))
  }

  low      <- low[low %in% x]
  high     <- high[high %in% x]
  mid      <- x[!x %in% c(high, low)]
  ordered  <- c(high, mid, low)

  return(ordered)
}
