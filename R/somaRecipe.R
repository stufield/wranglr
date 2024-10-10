#' Pre-processing SomaScan Data
#'
#' Relative too the \pkg{recipes} package, the [recipes::recipe()] and
#' [recipes::prep()] functions are combined.
#'
#' The order of recipe steps is *always*:
#' \enumerate{
#'   \item bridging (if `bridge_ref` passed)
#'   \item log10-transform
#'   \item center
#'   \item scale
#' }
#'
#' @inheritParams centerScaleData
#' @param data A `soma_adat` class object to use as a template for
#'   the pre-processing steps.
#' @param log10 Logical. Should `data` be log10-transformed.
#' @param bridge_ref Numeric. A vector of scale factors to bridge transform
#'   the data *prior* to applying the remaining steps in the recipe.
#'   They are passed to [scaleAnalytes()], and should be take the same
#'   format as the `scale_vec` argument. If `NULL` (default), no
#'   bridging is performed.
#' @param ... Optional arguments of the form `variable = function()`
#'   specifying the function to be applied to the specified column.
#'   Anonymous functions can be used but they should take a vector
#'   input and return a vector of equal length. See Examples.
#' @examples
#' # create a pre-processing recipe
#' rcp <- somaRecipe(sim_test_data)
#'
#' # additional processing of meta data variables
#' rcp2 <- somaRecipe(sim_test_data,
#'                    SiteId = factor,
#'                    reg_response = abs,
#'                    time = function(x) sqrt(x + pi))
#'
#' @importFrom SomaDataIO addClass getAnalytes
#' @export
somaRecipe <- function(data, log10 = TRUE, center = TRUE, scale = TRUE,
                       bridge_ref = NULL, ...) {
  if ( log10 ) {
    data <- log10(data)
    columns <- getAnalytes(data)
  } else {
    columns <- NA_character_
  }

  dots <- rlang::dots_list(...)
  list(     call = as.list(match.call(envir = baseenv())),
      bridge_lgl = !is.null(bridge_ref),
      bridge_ref = bridge_ref %||% NA_real_,
       log10_lgl = log10,
      log10_cols = columns,
      center_lgl = center,
       scale_lgl = scale,
               n = nrow(data),
               p = getAnalytes(data, n = TRUE),
         par_tbl = .genParTbl(data),
        dot_vars = names(dots)
  ) |>
  c(dots) |>
  addClass("soma_recipe")
}


#' S3 method for `soma_recipe` object
#' @noRd
#' @importFrom globalr pad signal_rule signal_done
#' @importFrom globalr signal_todo value liter symbl add_style
#' @export
print.soma_recipe <- function(x, ...) {
  writeLines(
    signal_rule("SomaScan pre-processing recipe", lty = "double",
                line_col = "magenta")
  )
  cat("\n")
  line3 <- paste0(rep.int(symbl$line, 3L), collapse = "")
  cat(line3, "Training data:\n")
  signal_done("Data containing",  value(x$n), "samples used in recipe")
  signal_done("RFU features ( n =", value(x$p), ") will be processed by:")
  cat("\n")

  cross  <- add_style$red(symbl$cross)
  tick   <- add_style$green(symbl$tick)
  .pad22 <- function(.x) pad(.x, width = 22)

  cat(line3, "Steps:\n")

  .pad22("Bridging") |> paste0(ifelse(x$bridge_lgl, tick, cross)) |>
    signal_todo()
  .pad22("log10-transformed") |> paste0(ifelse(x$log10_lgl, tick, cross)) |>
    signal_todo()
  .pad22("Centered (mean = 0)") |> paste0(ifelse(x$center_lgl, tick, cross)) |>
    signal_todo()
  .pad22("Scaled (sd = 1)") |>
    paste0(ifelse(x$scale_lgl, tick, cross)) |>
    signal_todo()
  cat("\n")
  # unknown arguments via `...`
  .dots <- x$call[x$dot_vars]
  if ( length(.dots) ) {
    signal_done("Additional processing:")
    cat("\n")
    liter(.dots, .f = function(.x, .y) {
      signal_todo(paste0(.pad22(.y), deparse(.x)))
    }) |> invisible()
  }
  writeLines(signal_rule(lty = "double", line_col = "green"))
  invisible(x)
}


#' @describeIn somaRecipe executes the recipe instructions defined during
#'   via [somaRecipe()].
#' @param x A `soma_recipe` class object with instructions for pre-processing.
#' @param data A `soma_adat` class object to be pre-process according
#'   to `x`.
#' @examples
#' # apply recipe to orig/own data set
#' new_data <- somaBake(rcp, sim_test_data)
#'
#' @importFrom globalr is.logspace
#' @export
somaBake <- function(x, data) {

  stopifnot(
    "`x` must be a 'soma_recipe' object."  = inherits(x, "soma_recipe"),
    "`.check_par_tbl(x$par_tbl)` failed."  = .check_par_tbl(x$par_tbl)
  )

  if ( x$bridge_lgl ) {
    ref  <- setNames(x$bridge_ref, getSeqId(names(x$bridge_ref)))
    data <- scale_features(data, ref)
  }

  if ( x$log10_lgl ) {
    if ( is.logspace(data) ) {
      warning("Double-logging danger! The `data` is already in log-space.",
              call. = FALSE)
    }
    columns <- x$log10_cols
    for ( col in columns ) {
      data[[col]] <- log10(data[[col]])
    }
  }

  # modify `...` variables in place
  for ( i in x$dot_vars ) {
    .fn <- x[[i]]
    data[[i]] <- .fn(data[[i]])
  }

  if ( x$scale_lgl || x$center_lgl ) {
    data <- centerScaleData(data, par_tbl = x$par_tbl,
                            center = x$center_lgl,
                            scale = x$scale_lgl)
  }
  structure(data, baked = TRUE)
}

#' @describeIn somaRecipe tests for presence of `baked` entry in
#'   attributes, indicating that these data have already been baked via
#'   [somaBake()].
#' @examples
#' # Logical test
#' is.baked(new_data)
#'
#' @export
is.baked <- function(data) {
  isTRUE(attr(data, "baked"))
}

#' @describeIn somaRecipe converts an existing recipe object from the
#'   \pkg{recipes} package into a `soma_recipe` object. Note that *all*
#'   conversions and not possible and the intended use case is for 3 step
#'   conversion:
#'   * log10-transform
#'   * centered
#'   * scaled
#' @param object A `recipe` class object from the `recipes` package with
#'   the 3 pre-processing steps described above.
#' @examples
#' # converting recipes
#' library(recipes)
#' rec <- recipe(Species ~ ., data = iris) |>
#'   step_log(all_predictors(), base = 10) |>
#'   step_center(all_predictors()) |>
#'   step_scale(all_predictors()) |>
#'   prep()
#'
#' convertRecipe(rec)
#' @importFrom tibble tibble enframe
#' @importFrom SomaDataIO addClass
#' @export
convertRecipe <- function(object) {

  if ( inherits(object, "soma_recipe") ) {
    return(object)
  }

  stopifnot(
    "`object` must be a `recipe` object." = inherits(object, "recipe")
  )

  ret <- list()
  predictors <- dplyr::filter(object$var_info,
                              role == "apts" |
                                role == "predictor" |
                                role == "predictors")$variable

  steps <- vapply(object$steps, function(.x) class(.x)[1L], "")
  idx   <- match(steps, "step_scale", nomatch = 0)

  # if 2 step_scale steps and first is in pos 1
  bridge_lgl <- sum(idx) == 2 && idx[1L] == 1
  # assume 1st step_scale is a bridge step
  # invert because scale is division
  bridge_ref <- 1 / object$steps[[1L]]$sds %||% NA_real_

  if ( bridge_lgl ) {
    object$steps[[1L]] <- NULL  # remove the bridging step
    steps <- steps[-1L]         # remove bridge step
  }

  ret$call <- match.call()
  ret$bridge_lgl <- bridge_lgl
  ret$bridge_ref <- bridge_ref
  ret$log10_lgl  <- "step_log" %in% steps
  ret$log10_cols <- NA_character_   # set default; update below
  ret$center_lgl <- "step_center" %in% steps
  ret$scale_lgl  <- "step_scale" %in% steps

  pars <- tibble(AptName = predictors)

  for ( step in steps ) {
    if ( step == "step_log" ) {
      logstep <- which(steps == "step_log")
      ret$log10_cols <- unname(object$steps[[logstep]]$columns)
    } else if ( step == "step_center" ) {
      df_mu <- enframe(getRecipeParams(object, "center"),
                       name = "AptName", value = "means")
      pars <- left_join(pars, df_mu, by = "AptName")
    } else if ( step == "step_scale" ) {
      df_sds <- enframe(getRecipeParams(object, "scale"),
                        name = "AptName", value = "sds")
      pars <- left_join(pars, df_sds, by = "AptName")
    }
  }

  if ( !"means" %in% names(pars) ) {
    pars$means <- 0   # no shift
  }
  if ( !"sds" %in% names(pars) ) {  # if no scaling
    pars$sds <- 1      # no scale
  }

  if ( !is.null(object$tr_info) ) {
    ret$n <- object$tr_info$nrows
  } else {
    ret$n <- nrow(object$template)  # likely bad for stripped recipes
  }

  ret$p        <- length(predictors)
  ret$par_tbl  <- pars
  ret$dot_vars <- character(0)
  addClass(ret, c("converted_recipe", "soma_recipe"))
}

#' update a `soma_recipe` control object.
#' @noRd
#' @export
update.soma_recipe <- function(object, ...) {
  # this needs work; sgf
  .dots <- list(...)
  for ( i in names(.dots) ) {
    object[[i]] <- .dots[[i]]
  }
  object
}

#' @noRd
#' @importFrom tibble deframe
#' @export
getRecipeParams.soma_recipe <- function(recipe, param = "scale") {
  param_name <- ifelse(param == "scale", "sds", "means")
  deframe(recipe$par_tbl[, c("AptName", param_name)])
}

#' @noRd
#' @export
getAnalytes.soma_recipe <- function(x, n = FALSE, rm.controls = FALSE) {
  vec <- x$par_tbl$AptName
  getAnalytes(vec, n = n, rm.controls = rm.controls)
}
