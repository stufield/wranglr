% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-recipe.R
\name{create_recipe}
\alias{create_recipe}
\alias{bake_recipe}
\alias{is.baked}
\alias{convert_recipe}
\title{Pre-processing Analysis Data}
\usage{
create_recipe(
  data,
  feat = NULL,
  log10 = TRUE,
  center = TRUE,
  scale = TRUE,
  ...
)

bake_recipe(x, data)

is.baked(data)

convert_recipe(object)
}
\arguments{
\item{data}{A \code{data.frame} to be pre-processed according to \code{x}.}

\item{feat}{\code{character(n)}. A vector indicating which
variables to center/scale.}

\item{log10}{\code{logical(1)}. Should features in \code{data} be log10-transformed?}

\item{center}{\code{logical(1)}. Whether the variables
should be shifted to be zero centered (\eqn{\mu = 0}).}

\item{scale}{\code{logical(1)}. Whether the variables
should be scaled to have unit variance (\eqn{\sigma = 1}).}

\item{...}{Optional arguments of the form \verb{variable = function()}
specifying the function to be applied to the specified column.
Anonymous functions can be used but they should take a vector
input and return a vector of equal length. See Examples. If also
center/scaling, the \code{...} transformation takes place \emph{before}
the center-scale step.}

\item{x}{A \code{rcp} class object with instructions for pre-processing.}

\item{object}{A \code{recipe} class object from the \code{recipes} package with
the 3 pre-processing steps described above.}
}
\value{
A \code{rcp} class object containing information
for the recipe steps.
}
\description{
Relative too the \pkg{recipes} package, the \code{\link[recipes:recipe]{recipes::recipe()}} and
\code{\link[recipes:prep]{recipes::prep()}} functions are combined.
}
\details{
The order of recipe steps is \emph{always}:
\enumerate{
\item log10-transform
\item center
\item scale
}
}
\section{Functions}{
\itemize{
\item \code{bake_recipe()}: executes the recipe instructions defined during via \code{\link[=create_recipe]{create_recipe()}}.

\item \code{is.baked()}: tests for presence of \code{baked} entry in attributes, indicating that
the data have already been baked via \code{\link[=bake_recipe]{bake_recipe()}}.

\item \code{convert_recipe()}: converts an existing recipe object from the
\pkg{recipes} package into a \code{rcp} object. Note that \emph{all}
conversions and not possible and the intended use case is for 3 step
conversion:
\itemize{
\item log10-transform
\item centered
\item scaled
}

}}
\examples{
# create a pre-processing recipe
rcp <- create_recipe(mtcars)
rcp

rcp2 <- create_recipe(mtcars,
                      feat = c("mpg", "disp", "drat", "wt"),
                      disp = abs,
                      hp   = log10,
                      qsec = function(x) round(x / 10))

# apply recipe to orig/own data set
new_data <- bake_recipe(rcp, mtcars)

# Logical test
is.baked(new_data)

# converting recipes
library(recipes)
rec <- recipe(Species ~ ., data = iris) |>
  step_log(all_predictors(), base = 10) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  prep()

convert_recipe(rec)
}
