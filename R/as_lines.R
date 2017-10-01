as_lines <- function(x, color = TRUE, ...){
  UseMethod("as_lines")
}



as_lines.data.frame <- function(
  x,
  color = TRUE,
  ...
){

  res <- rbind(
    t(as.matrix(names(x))),
    as.matrix(unname(x))
  )


  res <- apply(res, 2, function(x) stringi::stri_pad_left(x, max(nchar(x))))
  res <- apply(res, 1, paste, collapse = " ")

  if(color){
    res[1] <- style_colname(res[1])
  }

  res
}
