% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputeNAs.R
\name{imputeNAs}
\alias{imputeNAs}
\title{Impute NAs}
\usage{
imputeNAs(x)
}
\arguments{
\item{x}{A \code{data.frame}, matrix, or numeric vector.}
}
\value{
An object of the same dimension and class as \code{x}
with any \code{NA} values replaced with the median of the
corresponding vector.
}
\description{
Imputes any \code{NAs} with the median value.
Must be a numeric data only. \code{factor} or
\code{character} class are not allowed.
}
\examples{
df <- simdata
imputeNAs(df)

idx <- 25L  # random 25th column
imputeNAs(df[[idx]])

# test the S3 method
all.equal(imputeNAs(df[[idx]]), imputeNAs(df)[[idx]])

x <- df[[idx]]
x[seq(1, nrow(df), 10L)] <- NA_real_
x

median(x, na.rm = TRUE)

imputeNAs(x)

table(imputeNAs(x)) |> sort() |> tail()
}
\seealso{
\code{\link[=median]{median()}}

Other impute: 
\code{\link{impute_outliers}()},
\code{\link{impute_predictors}()}
}
\author{
Stu Field
}
\concept{impute}
