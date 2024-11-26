#' SeqId Look-Up Table
#'
#' @description
#'   Conveniently look up "annotation" information such as:
#'   \itemize{
#'     \item `EntrezGeneSymbol`
#'     \item `Target`
#'     \item `Dilution`
#'     \item `UniProt`
#'   }
#'
#'   keyed on a vector of `SeqId`s.
#'   The lookup dictionary is matched on values in the `SeqId`
#'   column of annotations, which accesses a static internal object.
#'   Alternatively, the user can explicitly pass an annotations table of
#'   of their choice.
#'   The [seqify()] function adds a `seq` class attribute allowing
#'   a convenient S3 print dispatch for interactive use.
#'
#' @details
#' If historical analytes have been dropped from the menu
#'   they will be absent from the default lookup table,
#'   resulting in `NA` for that row. But not all is lost! Dropped
#'   analytes can be retrieved by explicitly passing an annotations
#'   table corresponding to the original data prior to
#'   the menu change (see `Examples`). The easiest way to generate a
#'   "time-capsuled" annotations table:
#'      1. via `attr(data, "Col.Meta")`
#'
#' @family Annotations
#'
#' @param x,seq `character(n)`. A vector of `SeqIds`, or
#'   strings *containing* `SeqId`s.
#' @param tbl A tibble containing annotation data.
#'
#' @return For [seq_lookup()], a tibble, a subset of `tbl`,
#'   corresponding to the rows whose `SeqIds` match the values in `seq`.
#' @author Stu Field
#'
#' @seealso  [get_anno()], [left_join()]
#'
#' @examples
#' svec <- c("seq.2981.9", "seq.5073.30", "seq.4429.51", "seq.2447.7")
#' svec
#'
#' seq_lookup(svec)
#'
#' # can pass 'known' set of annotations
#' #   note: the NAs are now replaced with updated info
#' seq_lookup(svec, tbl = splyr:::apt_data)
#'
#' @importFrom tibble tibble as_tibble
#' @export
seq_lookup <- function(seq, tbl = NULL) {
  if ( is.null(tbl) ) {
    lookup <- get_anno()
  } else {
    lookup <- as_tibble(tbl)
    lookup$SeqId <- get_seq(lookup$SeqId)
  }
  add_vars <- c("UniProt", "List", "Reason")
  tibble(seq = seq, SeqId = get_seq(seq)) |>
    left_join(lookup, by = "SeqId") |>
    select(seq, SeqId, EntrezGeneSymbol, Target,
           TargetFullName, Type, Dilution, any_of(add_vars))
}


#' @describeIn seq_lookup
#'   Convert to `seq` object.
#' @return For `seqify()`, an object of class `seq`.
#' @examples
#' # print method for class `seq`
#' seqify(svec) |> class()
#'
#' # works with seq.xxxx.xx format
#' seqify(svec)
#'
#' # also works with pure SeqIds
#' vec <- sub("\\.", "-", sub("^seq\\.", "", svec))
#' vec
#'
#' seqify(vec)
#' @export
seqify <- function(x) {
  stopifnot("All values of `x` must be SeqIds." = all(is_seq(x)))
  structure(x, class = c("seq", "character"))
}


#' @noRd
#' @importFrom helpr add_color signal_rule liter
#' @export
print.seq <- function(x, ...) {
  tbl   <- seq_lookup(as.character(x))
  symb1 <- add_color("\u25b6", "cyan")
  symb2 <- add_color("  \u276F  ", "cyan")
  signal_rule("SeqId Lookup", lty = "double", line_col = "magenta")
  tbl  <- select(tbl, "SeqId-Feature" = "seq", GeneID = "EntrezGeneSymbol",
                 Target = "TargetFullName", "List", "Reason")
  tbl  <- rbind(names(tbl), tbl)
  cols <- c("red", "blue", "green", "yellow", "magenta")
  tbl_mat <- liter(tbl, cols, function(.x, .y) add_color(format(.x), .y)) |>
    data.frame() |> as.matrix()
  cat(paste(" ", tbl_mat[1L, ], " ", collapse = " "), "\n")
  signal_rule(line_col = "grey")
  tbl_mat <- tbl_mat[-1L, , drop = FALSE]
  tbl_mat[, 1L] <- paste(symb1, tbl_mat[, 1L])
  writeLines(apply(tbl_mat, 1L, paste, collapse = symb2))
  invisible(x)
}
