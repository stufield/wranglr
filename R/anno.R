#' Protein Annotations
#'
#' A group of functions that allow for the simple
#'  search and access of protein annotation information
#'  based on an internal lookup table.
#'
#' @name anno
#'
#' @param pattern `character(1)`. Regular expression pattern to match.
#' @param seq `character(n)`. A vector of `SeqIds`, or
#'   strings *containing* `SeqId`s.
#' @param tbl A `tibble` containing annotation data.
#'
#' @author Stu Field
NULL


#' @describeIn anno
#'   a convenient wrapper to easily retrieve the
#'   lookup table (`tibble`) of proteomic
#'   annotations keyed on `SeqId`.
#'
#' @return [get_anno()]: a `tibble` of protein annotation information.
#'
#' @examples
#' get_anno()
#'
#' @importFrom tibble as_tibble
#' @export
get_anno <- function() {
  as_tibble(annotations_v5.0)
}


#' @describeIn anno
#'   pattern matches via regular expression of
#'   the internal "annotations" table (`tibble`).
#'   Matches are performed on *any* (`regexpr = "|"`) of the following:
#'   \itemize{
#'     \item `EntrezGeneSymbol`
#'     \item `TargetFullName`
#'     \item `Target`
#'     \item `List`
#'     \item `SeqId`
#'   }
#'
#' @return [grep_anno()]: a `tibble` of a subset of
#'   protein information (annotations) corresponding
#'   to matching rows of in `pattern`.
#'
#' @examples
#' grep_anno("^MMP")     # match EntrezGeneSymbols
#' grep_anno("^29")      # match SeqId
#' grep_anno("^black")   # Blacklisted
#'
#' @export
grep_anno <- function(pattern) {
  dplyr::filter(
    get_anno(),
    grepl(pattern, Target) |
      grepl(pattern, TargetFullName) |
      grepl(pattern, EntrezGeneSymbol) |
      grepl(pattern, List) |
      grepl(pattern, SeqId)
  )
}


#' @describeIn anno
#'   conveniently looks up annotation keyed on
#'   a vector of `SeqId`s.
#'   The internal lookup dictionary is matched on values in the `SeqId`
#'   column of annotations, which accesses a static internal object.
#'   Alternatively, the user can explicitly pass an annotations table of
#'   of their choice.
#'
#' @details
#'   Historical features may have been dropped from the menu
#'   may be absent from the internal lookup table,
#'   resulting in `NA` for that row. In such cases,
#'   they can be retrieved by explicitly passing an annotations
#'   table corresponding to the original data prior to
#'   the menu change. The easiest way to generate a
#'   "time-capsuled" annotations table:
#'      1. via `attr(data, "Col.Meta")`
#'
#' @return [seq_lookup()]: a `tibble`, a subset of `tbl`,
#'   corresponding to the rows whose `SeqIds` match the values in `seq`.
#'
#' @examples
#' svec <- c("seq.2981.9", "seq.5073.30", "seq.4429.51", "seq.2447.7")
#' svec
#'
#' seq_lookup(svec)
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
  add_vars <- c("Dilution", "UniProt", "List", "Reason")
  tibble(seq = seq, SeqId = get_seq(seq)) |>
    left_join(lookup, by = "SeqId") |>
    select(seq, SeqId, EntrezGeneSymbol, Target,
           TargetFullName, any_of(add_vars))
}


#' @describeIn anno
#'   converts to `wranglr_seq` object, primarily to
#'   dispatch the S3 print method during interactive use.
#'
#' @return [seqify()]: a `wranglr_seq` class object.
#'
#' @examples
#' # print method for class `wranglr_seq`
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
#'
#' @export
seqify <- function(seq) {
  stopifnot("All values of `x` must contain `SeqIds`." = all(is_seq(seq)))
  structure(seq, class = c("wranglr_seq", "character"))
}


#' The S3 print method
#'
#' @noRd
#' @importFrom helpr add_color signal_rule liter
#' @export
print.wranglr_seq <- function(x, ...) {
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
