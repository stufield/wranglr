#' Look Up Annotations by Pattern
#'
#' Searches the internal "annotations" table (`tibble`) from
#'   regular expression matches according to `pattern`.
#'   Pattern matching is performed *any* (`regexpr = "|"`) of the following:
#'   \itemize{
#'     \item `EntrezGeneSymbol`
#'     \item `TargetFullName`
#'     \item `Target`
#'     \item `List`
#'     \item `SeqId`
#'   }
#'
#' @family Annotations
#'
#' @param pattern `character(1)`. Regular expression pattern to match.
#'
#' @return A tibble, a subset of Annotations from the Graph DB
#'   corresponding to matching rows of the desired `pattern`.
#' @author Stu Field
#'
#' @examples
#' lookup_anno("^MMP")     # match EntrezGeneSymbols
#' lookup_anno("^29")      # match SeqId
#' lookup_anno("^black")   # Blacklisted
#' @export
lookup_anno <- function(pattern) {
  dplyr::filter(
    annotations_v5.0,
    grepl(pattern, Target) |
      grepl(pattern, TargetFullName) |
      grepl(pattern, EntrezGeneSymbol) |
      grepl(pattern, List) |
      grepl(pattern, SeqId)
  )
}
