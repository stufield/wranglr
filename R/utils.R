
`%||%` <- function(x, y) {
  if ( is.null(x) || length(x) <= 0L ) {
    y
  } else {
    x
  }
}

is_seq <- getFromNamespace("is_seq", "helpr")
add_seq <- getFromNamespace("add_seq", "helpr")
get_analytes <- getFromNamespace("get_analytes", "helpr")
get_meta <- getFromNamespace("get_meta", "helpr")

