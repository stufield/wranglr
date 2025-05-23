% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-features.R
\name{scale_features}
\alias{scale_features}
\title{Scale/transform Features (variables)}
\usage{
scale_features(.data, scale_vec)
}
\arguments{
\item{.data}{A \code{data.frame} class object.}

\item{scale_vec}{A \emph{named} vector of scalars,
named by features in \code{.data}.}
}
\description{
Scale features by a scalar reference value (named vector)
match with the features contained in \code{.data}.
Columns without a corresponding reference value
are unmodified (with a warning).
}
\examples{
ref  <- c(mpg = 10.0, wt = 0.1, drat = 1.0, disp = 0.0)
new  <- scale_features(mtcars, ref)
new
}
\author{
Stu Field
}
