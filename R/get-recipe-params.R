#' Get Parameters From A Recipe
#'
#' Get mean and standard deviations used to
#'   center/scale data from a prepped recipe.
#'
#' @param recipe Prepped `recipe` or `rcp` class object.
#' @param param `character(1)`. Possible values are `center` or `scale`.
#'
#' @return A named numeric vector with means (`step = 'center'`)
#'   or standard deviations (`step = scale'`). Names correspond
#'   to recipe predictors.
#'
#' @examples
#' test  <- simdata
#' feats <- wranglr:::get_analytes(test)
#' rec <-  recipes::recipe(~ ., data = dplyr::select(test, dplyr::all_of(feats))) |>
#'  recipes::step_log(recipes::all_predictors(), base = 10) |>
#'  recipes::step_center(recipes::all_predictors()) |>
#'  recipes::step_scale(recipes::all_predictors()) |>
#'  recipes::prep(training = test)
#' get_recipe_params(rec, "scale")
#' get_recipe_params(rec, "center")
#'
#' rcp <- create_recipe(test)
#' get_recipe_params(rec, "center")
#'
#' get_recipe_params(rcp, "scale")
#' @export
get_recipe_params <- function(recipe, param) {
  if ( !param %in% c("scale", "center") ) {
    stop("Method not implemented for param '", param,
         "'. Possible values are 'scale' and 'center'.", call. = FALSE)
  }
  UseMethod("get_recipe_params")
}

#' @noRd
#' @export
get_recipe_params.default <- function(recipe, param) {
  stop(
    "Couldn't find a S3 method for this class object: ", value(class(recipe)),
    call. = FALSE
  )
}

#' @noRd
#' @importFrom stats setNames
#' @export
get_recipe_params.recipe <- function(recipe, param = "scale") {
  stepnames <- vapply(recipe$steps, function(step) class(step)[1L], "")
  steps <- setNames(recipe$steps, stepnames)

  rec_step <- steps[[paste0("step_", param)]]
  if ( is.null(rec_step) ) {
    warning("'recipe' has no '", param, "' step. Returning NULL.", call. = FALSE)
    return(NULL)
  }

  param_name <- ifelse(param == "scale", "sds", "means")
  rec_step[[param_name]]
}
