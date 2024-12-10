#' Select Distinct Rows
#'
#' Extract distinct rows by selection variables. Additional
#'   arguments allow specification of variables that are
#'   combined and checked for uniqueness.
#'
#' @param x A data frame or tibble object.
#' @param ... Additional arguments to [dplyr::select()].
#'
#' @return A data frame or tibble object, same class as `x`.
#'
#' @author Stu Field
#'
#' @examples
#' withr::with_seed(111, {
#'   df <- tibble::tibble(
#'     a_ie = sample(letters[1:3L], 20, replace = TRUE),
#'     b_ie = sample(1:3, 20, replace = TRUE),
#'     var  = runif(20)
#'   )
#' })
#' df
#'
#' distinct_rows(df, a_ie, b_ie)
#'
#' # or with tidyselect-helpers
#' distinct_rows(df, ends_with("ie"))
#' @importFrom helpr rm_rn
#' @export
distinct_rows <- function(x, ...) {
  sel <- dplyr::select(x, ...)
  rm_rn(x[!duplicated(sel), ])
}
