#' Pre-processing Analysis Data
#'
#' Relative too the \pkg{recipes} package, the [recipes::recipe()] and
#' [recipes::prep()] functions are combined.
#'
#' The order of recipe steps is *always*:
#' \enumerate{
#'   \item log10-transform
#'   \item center
#'   \item scale
#' }
#'
#' @inheritParams center_scale
#' @param data A `data.frame` object to use as a template for
#'   the pre-processing steps.
#' @param log10 Logical. Should features in `data` be log10-transformed?
#' @param ... Optional arguments of the form `variable = function()`
#'   specifying the function to be applied to the specified column.
#'   Anonymous functions can be used but they should take a vector
#'   input and return a vector of equal length. See Examples. If also
#'   center/scaling, the `...` transformation takes place *before*
#'   the center-scale step.
#' @return A `rcp` class object containing information for the recipe steps.
#' @examples
#' # create a pre-processing recipe
#' rcp <- create_recipe(mtcars)
#' rcp
#'
#' rcp2 <- create_recipe(mtcars,
#'                       feat = c("mpg", "disp", "drat", "wt"),
#'                       disp = abs,
#'                       hp   = log10,
#'                       qsec = function(x) round(x / 10))
#'
#' @export
create_recipe <- function(data, feat = NULL, log10 = TRUE, center = TRUE,
                          scale = TRUE, ...) {

  feat <- feat %||% names(which(vapply(data, is.numeric, NA)))
  dots <- rlang::dots_list(...)

  if ( length(dots) > 0L ) {
    data <- .apply_dot_funcs(data, dots)   # this happens first
  }

  if ( log10 ) {
    data[, feat] <- log10(data[, feat])
  }

  list(     call = as.list(match.call(envir = baseenv())),
      features   = feat,
       log10_lgl = log10,
      center_lgl = center,
       scale_lgl = scale,
               n = nrow(data),
               p = length(feat),
         par_tbl = .genParTbl(data, feat),
        dot_vars = names(dots)
  ) |>
  c(dots) |>
  structure(class = c("rcp", "list"))
}

# helper function to apply functions for `...`
# to the data modified in place during bake and prep
.apply_dot_funcs <- function(data, dots) {
  for ( i in names(dots) ) {
    .f <- dots[[i]]
    data[[i]] <- .f(data[[i]])
  }
  data
}


#' S3 method for `rcp` object
#' @noRd
#' @importFrom helpr pad signal_rule signal_done
#' @importFrom helpr signal_todo value liter symbl add_style
#' @export
print.rcp <- function(x, ...) {
  writeLines(
    signal_rule("Pre-processing recipe", lty = "double", line_col = "magenta")
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


#' @describeIn create_recipe
#'   executes the recipe instructions defined during via [create_recipe()].
#' @param x A `rcp` class object with instructions for pre-processing.
#' @param data A `data.frame` to be pre-processed according to `x`.
#' @examples
#' # apply recipe to orig/own data set
#' new_data <- bake_recipe(rcp, mtcars)
#'
#' @export
bake_recipe <- function(x, data) {

  stopifnot(
    "`x` must be a 'rcp' object."          = inherits(x, "rcp"),
    "`.check_par_tbl(x$par_tbl)` failed."  = .check_par_tbl(x$par_tbl)
  )

  # modify `...` variables in place
  # this must happen *first*
  if ( length(x$dot_vars) > 0L ) {
    data <- .apply_dot_funcs(data, x[x$dot_vars])
  }

  if ( x$log10_lgl ) {
    columns <- x$features
    stopifnot("Features missing in `data`." = all(columns %in% names(data)))
    for ( col in columns ) {
      data[[col]] <- log10(data[[col]])
    }
  }

  if ( x$scale_lgl || x$center_lgl ) {
    data <- center_scale(data,
                         par_tbl = x$par_tbl,
                         feat    = columns,
                         center  = x$center_lgl,
                         scale   = x$scale_lgl)
  }
  structure(data, baked = TRUE)
}

#' @describeIn create_recipe
#'   tests for presence of `baked` entry in attributes, indicating that
#'   the data have already been baked via [bake_recipe()].
#' @examples
#' # Logical test
#' is.baked(new_data)
#'
#' @export
is.baked <- function(data) {
  isTRUE(attr(data, "baked"))
}

#' @describeIn create_recipe
#'   converts an existing recipe object from the
#'   \pkg{recipes} package into a `rcp` object. Note that *all*
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
#' convert_recipe(rec)
#' @importFrom tibble tibble enframe
#' @importFrom helpr add_class
#' @export
convert_recipe <- function(object) {

  if ( inherits(object, "rcp") ) {
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

  ret$call <- match.call()
  ret$features   <- predictors
  ret$log10_lgl  <- "step_log" %in% steps
  ret$center_lgl <- "step_center" %in% steps
  ret$scale_lgl  <- "step_scale" %in% steps

  pars <- tibble(feature = predictors)

  for ( step in steps ) {
    if ( step == "step_log" ) {
      logstep <- which(steps == "step_log")
      # this step doesn't do much yet
      feats <- unname(object$steps[[logstep]]$columns)
    } else if ( step == "step_center" ) {
      df_mu <- enframe(get_recipe_params(object, "center"),
                       name = "feature", value = "means")
      pars <- left_join(pars, df_mu, by = "feature")
    } else if ( step == "step_scale" ) {
      df_sds <- enframe(get_recipe_params(object, "scale"),
                        name = "feature", value = "sds")
      pars <- left_join(pars, df_sds, by = "feature")
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
  structure(ret, class = c("converted_recipe", "rcp", "list"))
}

#' update a `rcp` control object.
#' @noRd
#' @export
update.rcp <- function(object, ...) {
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
get_recipe_params.rcp <- function(recipe, param = "scale") {
  param_name <- ifelse(param == "scale", "sds", "means")
  deframe(recipe$par_tbl[, c("feature", param_name)])
}
