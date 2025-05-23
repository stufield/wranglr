% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-recipe-params.R
\name{get_recipe_params}
\alias{get_recipe_params}
\title{Get Parameters From A Recipe}
\usage{
get_recipe_params(recipe, param)
}
\arguments{
\item{recipe}{Prepped \code{recipe} or \code{rcp} class object.}

\item{param}{\code{character(1)}. Possible values are \code{center} or \code{scale}.}
}
\value{
A named numeric vector with means (\code{step = 'center'})
or standard deviations (\verb{step = scale'}). Names correspond
to recipe predictors.
}
\description{
Get mean and standard deviations used to
center/scale data from a prepped recipe.
}
\examples{
test  <- simdata
feats <- wranglr:::get_analytes(test)
rec <-  recipes::recipe(~ ., data = dplyr::select(test, dplyr::all_of(feats))) |>
 recipes::step_log(recipes::all_predictors(), base = 10) |>
 recipes::step_center(recipes::all_predictors()) |>
 recipes::step_scale(recipes::all_predictors()) |>
 recipes::prep(training = test)
get_recipe_params(rec, "scale")
get_recipe_params(rec, "center")

rcp <- create_recipe(test)
get_recipe_params(rec, "center")

get_recipe_params(rcp, "scale")
}
