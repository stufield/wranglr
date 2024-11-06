#' Scale/transform Analyte RFUs
#'
#' Scale features by a scalar reference value (named vector)
#'   match with the features contained in `.data`.
#'   Columns without a corresponding reference value
#'   are unmodified (with a warning).
#'
#' @param .data A `data.frame` class object.
#' @param scale_vec A *named* vector of scalars, named by features in `.data`.
#'
#' @author Stu Field
#'
#' @examples
#' ref  <- c(mpg = 10.0, wt = 0.1, drat = 1.0, disp = 0.0)
#' new  <- scale_features(mtcars, ref)
#' new
#' @importFrom tibble enframe deframe
#' @export
scale_features <- function(.data, scale_vec) {

  stopifnot(
    "`scale_vec` must be a named numeric vector." =
      !is.null(scale_vec) && is.numeric(scale_vec),
    "`.data` must be a `data.frame`." = is.data.frame(.data)
  )

  stbl    <- enframe(scale_vec, "feature")
  feats   <- names(vapply(.data, is.numeric, NA))
  common  <- intersect(feats, stbl$feature)
  matches <- data.frame(df_name  = common,
                        vec_name = intersect(common, stbl$feature))
  if ( nrow(matches) == 0L ) {
    warning("No matches in `scale_vec` ... returning original data", call. = FALSE)
    return(.data)
  }
  missing <- setdiff(feats, matches$df_name)
  extra   <- setdiff(stbl$feature, matches$vec_name)

  if ( length(missing) > 0L ) {
    # if more entries in the data
    warning(
      "Missing scalar value for (", length(missing), ") features. ",
      "They will not be transformed.\n",
      "Please check the reference or its names.",
      call. = FALSE
    )
  }

  if ( length(extra) > 0L ) {
    # if more entries in the reference
    warning(
      "There are extra scaling values (", length(extra), ") in the reference.\n",
      "They will be ignored.", call. = FALSE
    )
    stbl <- dplyr::filter(stbl, feature %in% matches$vec_name)
  }

  svec <- deframe(stbl)           # return to named vector
  svec <- svec[matches$vec_name]  # order reference to data

  # apply svec scalars by column
  new <- .data[, matches$df_name, drop = FALSE] |> .transform(unname(svec))
  .data[, matches$df_name] <- data.frame(new, row.names = NULL)
  .data
}


# do not use the generic transform()
.transform <- function(.data, v, dim = 2L, ...) {
  stopifnot(dim %in% 1:2L)
  x <- .data
  if ( dim == 2L ) {
    stopifnot(length(v) == ncol(x))
    t(t(x) * v)
  } else {
    stopifnot(length(v) == nrow(x))
    as.matrix(x) * v
  }
}
