#' Match Two Data Frames Based on Sample ID
#'
#' Given two data frames, this function matches their rows
#'   based on the intersection of sample IDs in `idcol`.
#'   The returned data frames are ordered by `idcol`.
#'   Rows that do not have intersecting `idcol` values are excluded.
#'
#' @param x,y The, two, `data.frame` objects to match against each other.
#' @param idcol Character. The name of the column in both `x` and `y`
#'   to use for matching and ordering the rows of the data frames.
#' @return A named list with `x` and `y`, matched and ordered
#'   based on `idcol`.
#' @examples
#' df    <- head(sim_test_data, 25L)
#' train <- withr::with_seed(1, dplyr::sample_n(df, 15L))
#' new   <- withr::with_seed(2, dplyr::sample_n(df, 15L))
#' intersect(train$SampleId, new$SampleId)    # there are overlapping IDs
#'
#' dfs <- match_samples(train, new)
#' dfs
#' @importFrom globalr value
#' @export
match_samples <- function(x, y, idcol = "SampleId") {
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  stopifnot(
    "`x` must be a `data.frame`." = is.data.frame(x),
    "`y` must be a `data.frame`." = is.data.frame(y),
    "`idcol` must be a variable in `x`." = idcol %in% names(x),
    "`idcol` must be a variable in `y`." = idcol %in% names(y)
  )

  shared_ids <- intersect(x[[idcol]], y[[idcol]])
  n <- length(shared_ids)
  if ( n == 0L ) {
    stop(
      "`", xname, "` and `", yname, "` have no samples in common with idcol ",
      value(idcol), ".", call. = FALSE
    )
  }

  id_sym <- rlang::ensym(idcol)
  df_x <- arrange(dplyr::filter(x, !!id_sym %in% shared_ids), !!id_sym)
  df_y <- arrange(dplyr::filter(y, !!id_sym %in% shared_ids), !!id_sym)

  if ( nrow(df_x) != n || nrow(df_y) != n ) {
    stop(
      "Duplicate sample IDs exist in `", xname, "` or `", yname, "`.",
      call. = FALSE
    )
  }

  list(x = df_x, y = df_y)
}
