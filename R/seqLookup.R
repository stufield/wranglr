#' SeqId Look-Up Table
#'
#' @description
#' Conveniently look up "annotation" information such as:
#'   * `EntrezGeneSymbol`
#'   * `Target`
#'   * `Dilution`
#'   * `UniProt`
#'
#' keyed on a vector of `SeqId`s.
#' The lookup dictionary is matched on values in the `SeqId`
#' column of Annotations (from the Graph Database), which is
#' accessed via a static internal object.
#' See [getAnnotations()] for details.
#' Alternatively, the user can explicitly pass an annotations table of
#' of their choice.
#' The [seqify()] function adds a `seqId` class attribute allowing
#' for a convenient S3 print method intended for interactive use.
#'
#' @details
#' If historical analytes have been dropped from the menu
#'   they will be absent from the default lookup table,
#'   resulting in `NA` for that row. But not all is lost! Dropped
#'   analytes can be retrieved by explicitly passing an annotations
#'   table corresponding to the original `soma_adat` prior to
#'   the menu change (see `Examples`). There are 2 ways to generate a
#'   "time-capsuled" annotations table:
#'      1. via `getAnalyteInfo()`
#'      1. via `attr(adat, "Col.Meta")`
#'
#' @family Annotations
#' @param x,seq Character. A vector of `SeqIds`, `AptNames`, or `Aptamers`, all
#'   containing `SeqId`s.
#' @param tbl A tibble containing analyte (annotation) data.
#'   Typically generated via calls to `getAnalyteInfo()`.
#' @return For [seqLookup()], a tibble, a subset of `tbl`,
#'   corresponding to the rows whose `SeqIds` match the values in `seq`.
#' @author Stu Field
#' @seealso  [getAnnotations()], [left_join()]
#' @examples
#' svec <- c("seq.2981.9", "seq.5073.30", "seq.4834.61", "seq.5006.71",
#'           "seq.3213.65", "seq.3352.80", "seq.4429.51", "seq.2447.7")
#' svec
#'
#' seqLookup(svec)
#'
#' # user can pass 'known' set of annotations
#' # Note: the NAs are now replaced with accurate info
#' tbl <- splyr:::apt_data
#' seqLookup(svec, tbl)
#'
#' @importFrom tibble tibble
#' @export
seqLookup <- function(seq, tbl = NULL) {
  if ( !is.null(tbl) ) {
    lookup <- dplyr::ungroup(tbl)
    lookup$SeqId <- getSeqId(lookup$SeqId, trim.version = TRUE)
  } else {
    lookup <- getAnnotations(api = FALSE)
  }
  vars <- intersect(c("UniProt", "List", "Reason"), names(lookup))
  tibble(seq = seq, SeqId = getSeqId(seq, TRUE)) |>
    left_join(lookup, by = "SeqId") |>
    select(seq, SeqId, EntrezGeneSymbol, Target,
           TargetFullName, Type, Dilution, !!vars)
}

#' Convert to `seqId` object
#' @name seqLookup
#' @return For `seqify()`, an object of class `seqId`.
#' @examples
#' # print method for class `seqId`
#' class(seqify(svec))
#'
#' # works with anonymous-AptNames
#' seqify(svec)
#'
#' # also works with naked-SeqIds
#' seqify(globalr:::getSeqId(svec))
#' @importFrom globalr add_class
#' @export
seqify <- function(x) {
  stopifnot("All values of `x` must be SeqIds." = all(globalr:::is.apt(x)))
  add_class(x, "seqId")
}

#' @importFrom globalr add_color signal_rule liter
#' @noRd
#' @export
print.seqId <- function(x, ...) {
  tbl   <- seqLookup(as.character(x))
  symb1 <- add_color("\u25b6", "cyan")
  symb2 <- add_color("  \u276F  ", "cyan")
  writeLines(
    signal_rule("SeqId Lookup", lty = "double", line_col = "magenta")
  )
  tbl  <- select(tbl, "SeqId-AptName" = "seq", GeneID = "EntrezGeneSymbol",
                 Target = "TargetFullName", "List", "Reason")
  tbl  <- rbind(names(tbl), tbl)
  cols <- c("red", "blue", "green", "yellow", "magenta")
  tbl_mat <- liter(tbl, cols, function(.x, .y) add_color(format(.x), .y)) |>
    data.frame() |> as.matrix()
  cat(paste(" ", tbl_mat[1L, ], " ", collapse = " "), "\n")
  writeLines(signal_rule(line_col = "grey"))
  tbl_mat <- tbl_mat[-1L, , drop = FALSE]
  tbl_mat[, 1L] <- paste(symb1, tbl_mat[, 1L])
  writeLines(apply(tbl_mat, 1L, paste, collapse = symb2))
  invisible(x)
}
