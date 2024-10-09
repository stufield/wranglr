#' Center and/or Scale Data
#'
#' A function to center and/or scale a data matrix (RFU table). Can be applied
#' to a `soma_adat`, tibble, data frame, or matrix object, but if a
#' matrix, should contain *only* numeric columns (e.g. RFU data).
#'
#' @param data A `soma_adat`, tibble, data frame, or matrix object with
#'   named RFU data to center and/or scale.
#' @param par_tbl A tibble containing the mean and standard deviations
#'   to use in processing the data. Must also contain an `AptName` column
#'   to synchronize the features with their corresponding scaling parameters.
#'   If `NULL`, a parameter table is generated based on `data`,
#'   i.e. `data` is its own reference.
#' @param center Logical. Indicating whether the variables
#'   should be shifted to be zero centered (\eqn{\mu = 0}).
#' @param scale Logical. Indicating whether the variables
#'   should be scaled to have unit variance (\eqn{\sigma = 1}).
#' @param ref.data Discouraged. Now preferred to pass `par_tbl`, however
#'   maintained for backward compatibility. A data set used to
#'   calculate `par_tbl`, i.e. vectors of means and standard deviations
#'   will be calculated from *this* data set and applied to `data`.
#' @return A center/scaled object of the same class as `data`. Only features
#'   are modified for non-`matrix` objects.
#' @author Stu Field
#' @examples
#' scaled <- centerScaleData(sim_test_data)
#' apply(strip_meta(scaled), 2, mean) |> sum()  # mean = 0
#' apply(strip_meta(scaled), 2, sd)             # sd = 1
#'
#' # Pass parameters based on OTHER data
#' idx   <- withr::with_seed(1,
#'   sample(1:nrow(sim_test_data), size = nrow(sim_test_data) / 2)
#' )
#' train <- sim_test_data[idx, ]
#' test  <- sim_test_data[-idx, ]
#' new   <- centerScaleData(test, ref.data = train)
#'
#' # However, it is preferred to pass `par_tbl` over `ref.data`
#' #   by creating a `par_tbl` object based on `train`
#' par <- tibble::tibble(AptName = SomaDataIO::getAnalytes(train),
#'                       means   = colMeans(strip_meta(train)),
#'                       sds     = apply(strip_meta(train), 2, sd))
#' new2 <- centerScaleData(test, par_tbl = par)
#' @export
centerScaleData <- function(data, par_tbl = NULL, center = TRUE, scale = TRUE,
                            ref.data = deprecated()) {
  UseMethod("centerScaleData")
}

#' @noRd
#' @export
centerScaleData.default <- function(data, par_tbl = NULL, center = TRUE,
                                    scale = TRUE, ref.data = deprecated()) {
  stop("No S3 method could be found for object of class: ",
       value(class(data)), call. = FALSE)
}

#' @noRd
#' @importFrom lifecycle is_present deprecated deprecate_warn
#' @importFrom purrr pmap
#' @importFrom SomaDataIO getAnalytes
#' @export
centerScaleData.soma_adat <- function(data, par_tbl = NULL, center = TRUE,
                                      scale = TRUE, ref.data = deprecated()) {

  if ( is_present(ref.data) ) {
    deprecate_warn(
      "4.2.0",
      "splyr::centerScaleData(ref.data =)",
      "splyr::centerScaleData(par_tbl =)",
      details = "Passing 'ref.data =' is now discouraged."
    )
  }

  if ( is_present(ref.data) && !is.null(par_tbl) ) {
    stop("You cannot pass both `ref.data` AND `par_tbl`.", call. = FALSE)
  }

  if ( is_present(ref.data) ) {
    # if ref.data IS passed -> use it
    par_tbl <- .genParTbl(ref.data)
  } else if ( is.null(par_tbl) ) {
    # generate par_tbl on-the-fly ref.data not passed
    par_tbl <- .genParTbl(data)
  } else {
    # if par_tbl IS passed, must check its format
    if ( !.check_par_tbl(par_tbl) ) {
      stop("Please check the `par_tbl` passed to `centerScaleData()`.",
           call. = FALSE)
    }
    # ensure data and par_tbl are sync'd
    # if user passed, could be out of order
    par_tbl <- rearrange(par_tbl, "AptName", getAnalytes(data))
  }

  apts <- par_tbl$AptName
  pars <- list(data  = data[, apts],
               means = par_tbl$means,
               sds   = par_tbl$sds)

  if ( center && scale ) {
    ret_data <- pmap(pars, function(data, means, sds) (data - means) / sds)
  } else if ( center ) {
    ret_data <- pmap(pars, function(data, means, ...) (data - means))
  } else if ( scale ) {
    ret_data <- pmap(pars, function(data, sds, ...) data / sds)
  } else {
    stop("At least 1 of 'center' or 'scale' must be `TRUE`.", call. = FALSE)
  }
  data[, apts] <- as.data.frame(ret_data) # replace un-scaled -> scaled

  if ( !(center && scale) ) {
    par_tbl$means <- 0
    par_tbl$sds   <- 1
  }

  structure(data, par_tbl = par_tbl, center_lgl = center, scale_lgl = scale)
}

#' @noRd
#' @export
centerScaleData.tr_data <- centerScaleData.soma_adat

#' @noRd
#' @export
centerScaleData.data.frame <- centerScaleData.soma_adat

#' @noRd
#' @export
centerScaleData.tbl_df <- centerScaleData.soma_adat

#' @noRd
#' @export
centerScaleData.matrix <- function(data, par_tbl = NULL, center = TRUE,
                                   scale = TRUE, ref.data = deprecated()) {
  if ( !is.null(ref.data) ) {
    if ( center ) {
      center <- colMeans(data, na.rm = TRUE)
    }
    if ( scale ) {
      scale  <- apply(data, 2, stats::sd, na.rm = TRUE)
    }
  }
  scale(data, center = center, scale = scale)
}


#' @describeIn centerScaleData tests for presence of `par_tbl` entry in
#' attributes and if it contains appropriate parameter information that
#' can be used for centering or scaling data.
#' @examples
#' # Logical test
#' is.centerScaled(new)
#' @export
is.centerScaled <- function(data) {
  x <- attr(data, "par_tbl")
  scaled <- !is.null(x)
  scaled && .check_par_tbl(x)
}


#' @describeIn centerScaleData the inverse of `centerScaleData()`. Undo the
#' transformation.
#' @examples
#' # Example of `undoCenterScale()`; reverse above
#' old <- undoCenterScale(new)
#'
#' # check values are reverted
#' all.equal(test, old)
#'
#' @importFrom stats sd
#' @importFrom purrr pmap
#' @importFrom SomaDataIO getAnalytes
#' @export
undoCenterScale <- function(data) {
  # do some checking
  # $par_tbl element added in `centerScaleData()`; required below
  stopifnot(is.centerScaled(data))
  apts    <- getAnalytes(data)
  par_tbl <- attr(data, "par_tbl") |> rearrange("AptName", apts)  # sync order
  stopifnot(.check_par_tbl(par_tbl))
  center  <- attr(data, "center_lgl")
  scale   <- attr(data, "scale_lgl")
  pars    <- as.list(par_tbl)  # convert list
  pars$data <- data[, apts]    # add `data`

  if ( center && scale ) {
    ret_data <- pmap(pars, function(data, means, sds, ...) data * sds + means)
  } else if ( center ) {
    ret_data <- pmap(pars, function(data, means, ...) data + means)
  } else if ( scale ) {
    ret_data <- pmap(pars, function(data, sds, ...) data * sds)
  } else {
    stop(
      "The `par_tbl` attribute is wrong. Check ",
      value("attr(data, \"par_tbl\")"), call. = FALSE
    )
  }

  data[, apts] <- data.frame(ret_data)  # replace un-scaled -> scaled data
  # remove scaling parameters so you cannot 'double' undo
  structure(data, par_tbl = NULL, center_lgl = NULL, scale_lgl = NULL)
}


#' checking function for the parameter table format
#' @noRd
.check_par_tbl <- function(x) {
  # a tibble with AptName present
  a <- inherits(x, "tbl_df") && ("AptName" %in% names(x))
  # must have at least 1 present
  b <- "means" %in% names(x) || "sds" %in% names(x)
  # if present -> must be double; don't fail if absent; `b` above covers that
  mtype <- ifelse("means" %in% names(x), typeof(x$means) == "double", TRUE)
  stype <- ifelse("sds" %in% names(x), typeof(x$sds) == "double", TRUE)
  (a && b && mtype && stype)
}


#' Generate the parameter table of values for centering and scaling, i.e.
#' means and standard deviations.
#' @importFrom tibble tibble
#' @noRd
.genParTbl <- function(x) {
  tibble(
    AptName = getAnalytes(x),
    means   = unname(colMeans(strip_meta(x), na.rm = TRUE)),
    sds     = unname(apply(strip_meta(x), 2, stats::sd, na.rm = TRUE))
  )
}
