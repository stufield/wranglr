#' Look Up Aptamer Pattern
#'
#' Searches the internal "Annotations" table (`tibble`) from
#' the Graph Database for regular expression matches according to `pattern`.
#' Pattern matching is performed *any* (`regexpr = "|"`) of the following:
#'   * `EntrezGeneSymbol`
#'   * `TargetFullName`
#'   * `Target`
#'   * `List`
#'   * `SeqId`
#'
#' @family Annotations
#' @param pattern Character. Regular expression pattern to match.
#' @return A tibble, a subset of Annotations from the Graph DB
#'   corresponding to matching rows of the desired `pattern`.
#'   An empty `tibble` if no matches found.
#' @author Stu Field
#' @examples
#' lookupAnnotations("^MMP")     # match EntrezGeneSymbols
#' lookupAnnotations("^29")      # match SeqId
#' lookupAnnotations("^black")   # Blacklisted
#' @export
lookupAnnotations <- function(pattern) {
  dplyr::filter(
    annotations_v5.0,
    grepl(pattern, Target) |
      grepl(pattern, TargetFullName) |
      grepl(pattern, EntrezGeneSymbol) |
      grepl(pattern, List) |
      grepl(pattern, SeqId)
  )
}
