#' Match Two Data Frames Based on Sample ID
#'
#' Given two data frames, this function matches their rows
#'   based on the intersection of a unique sample identifier column,
#'   `idcol`.
#'   The returned data frames are ordered by `idcol` and containing
#'   only rows based on the intersection of `idcol` values.
#'
#' @param x,y The two data frames to match samples against.
#' @param idcol `character(1)`. The name of the column containing
#'   sample identifiers in both `x` and `y`.
#'
#' @return A named list with `x` and `y`, matched and ordered
#'   based on `idcol`.
#'
#' @examples
#' df    <- head(simdata, 25L)
#' train <- withr::with_seed(1, dplyr::sample_n(df, 15L))
#' new   <- withr::with_seed(2, dplyr::sample_n(df, 15L))
#' intersect(train$SampleId, new$SampleId)  # overlapping IDs exist
#'
#' dfs <- match_samples(train, new)
#' dfs
#' @importFrom helpr value
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

  df_x <- dplyr::filter(x, x[[idcol]] %in% shared_ids) |>
    arrange(!!str2lang(idcol))
  df_y <- dplyr::filter(y, y[[idcol]] %in% shared_ids) |>
    arrange(!!str2lang(idcol))

  if ( nrow(df_x) != n || nrow(df_y) != n ) {
    stop(
      "Duplicate sample IDs exist in `", xname, "` or `", yname, "`.",
      call. = FALSE
    )
  }

  set_Names(list(df_x, df_y), c(xname, yname))
}
