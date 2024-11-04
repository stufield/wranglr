#' Get Database Annotations
#'
#' A convenient wrapper to easily retrieve the
#'   lookup table (tibble) of proteomic
#'   annotations keyed on `SeqId`.
#'
#' @return A tibble.
#' @examples
#' get_anno()
#' @author Stu Field
#' @importFrom tibble as_tibble
#' @export
get_anno <- function() {
  as_tibble(annotations_v5.0)
}

