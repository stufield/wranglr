
`%||%` <- function(x, y) {
  if ( is.null(x) || length(x) <= 0L ) {
    y
  } else {
    x
  }
}

is.apt <- getFromNamespace("is.apt", "helpr")
seqid2apt <- getFromNamespace("seqid2apt", "helpr")
getAnalytes <- getFromNamespace("getAnalytes", "helpr")
getMeta <- getFromNamespace("getMeta", "helpr")

