
`%||%` <- function(x, y) {
  if ( is.null(x) || length(x) <= 0L ) {
    y
  } else {
    x
  }
}

is.apt <- getFromNamespace("is.apt", "globalr")
seqid2apt <- getFromNamespace("seqid2apt", "globalr")
getAnalytes <- getFromNamespace("getAnalytes", "globalr")
getMeta <- getFromNamespace("getMeta", "globalr")

