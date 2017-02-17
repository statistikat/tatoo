#
# mash_cols_tex <- function(dat) {
#   foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
#     r <- paste(dat[[1]][i, ], dat[[2]][i, ], sep = ' ') %>%
#       t() %>%
#       as.data.frame()
#     names(r) <-  names(dat[[1]])
#
#     return(r)
#   }
# }
#
#
# mash_rows_tex <- function(dat, insert_blank_row) {
#   dat %assert_class% 'Mash_table'
#
#   empty_row <- rep('', length(dat[[1]])) %>%
#     t() %>%
#     as.data.frame(stringsAsFactors = FALSE) %>%
#     data.table::setnames(names(dat[[2]]))
#
#   res <- foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
#     r <- paste(dat[[1]][i, ], dat[[2]][i, ], sep = ' \\newline ') %>%
#       t() %>%
#       as.data.frame()
#     names(r) <-  names(dat[[1]])
#
#
#     if (insert_blank_row && i != nrow(dat[[1]])) {
#       r <- rbind(r, empty_row)
#     }
#
#     return(r)
#   }
# }
#
