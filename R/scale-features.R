#' Scale/transform Analyte RFUs
#'
#' Scale features by a scalar reference value (named vector)
#' match with the features contained in `.data`. Columns without a
#' corresponding reference value are unmodified (with a warning).
#'
#' @param .data A `data.frame` class object.
#' @param scale_vec A *named* vector of scalars, named by features in `.data`.
#' @author Stu Field
#' @examples
#' ref  <- c(mpg = 10.0, wt = 0.1, drat = 1.0, disp = 0.0)
#' new  <- scale_features(mtcars, ref)
#' new
#' @importFrom tibble enframe deframe
#' @importFrom SomaDataIO getAnalytes getSeqIdMatches getSeqId is.soma_adat
#' @export
scale_features <- function(.data, scale_vec) {

  .code <- function(x) {
    paste0("\033[90m", encodeString(x, quote = "`"), "\033[39m")
  }

  oc <- class(.data)

  stopifnot(
    "`scale_vec` must be a named numeric vector." =
      !is.null(scale_vec) && is.numeric(scale_vec),
    "`.data` must be a `data.frame`." = is.data.frame(.data)
  )

  stbl <- enframe(scale_vec, "feature")

  if ( is.soma_adat(.data) ) {
    feats <- getAnalytes(.data)
    matches <- getSeqIdMatches(feats, stbl$feature) |> # order matters; apts 1st
      setNames(c("data_name", "vec_name"))
  } else {
    feats   <- names(.data)
    common  <- intersect(feats, stbl$feature)
    matches <- data.frame(data_name = common,
                          vec_name  = intersect(common, stbl$feature))
    .data <- addClass(.data, "scale_df")
  }
  missing <- setdiff(feats, matches$data_name)
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
  new <- transform(.data[, matches$data_name, drop = FALSE], unname(svec))
  .data[, matches$data_name] <- data.frame(new, row.names = NULL)
  structure(.data, class = oc)
}


# This is a shim for data.frame class
# but without stepping on the existing one
# Simply create a dummy class and a method for it
transform.scale_df <- function(`_data`, v, dim = 2L, ...) {
  stopifnot(dim %in% 1:2L)
  x <- `_data`
  if ( dim == 2L ) {
    stopifnot(length(v) == ncol(x))
    t(t(x) * v)
  } else {
    stopifnot(length(v) == nrow(x))
    as.matrix(x) * v
  }
}
