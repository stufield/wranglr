#' Impute Outlier Values
#'
#' Given a numeric vector, this function imputes outlier values,
#'   defined as \eqn{3 \times \sigma} from the mean, back to a
#'   robustly calculated Gaussian. Gaussian parameters (\eqn{\mu + \sigma})
#'   are robustly calculated.
#'
#'   The maximum value, i.e. the \eqn{100^{th}} percentile, is pushed back
#'   to the \eqn{3 \times \sigma} value of the Gaussian.
#'
#' @family impute
#' @inheritParams helpr::get_outliers
#'
#' @param x A vector of values, approximating a Gaussian distribution and
#'   containing (possibly) outlier samples.
#'
#' @return A vector of values approximating a Gaussian
#'   distribution with the outlier samples imputed back
#'   to the robust Gaussian fit.
#'
#' @author Stu Field
#' @seealso [pnorm()], [qnorm()], [get_outliers()]
#'
#' @examples
#' # Gaussian with 4 outliers (2hi, 2lo)
#' vec  <- withr::with_seed(101, c(2, 2.5, rnorm(26, 15, 2), 25, 25.9))
#' pars <- attributes(helpr::get_outliers(vec, n_sigma = 3, type = "para"))
#' pars
#'
#' impute_outliers(vec)
#'
#' # Plot what is happening:
#' # original `vec` and critical 3*sd cutoffs in `red`
#' library(ggplot2)
#' range <- seq(min(vec), max(vec), length.out = 100)
#' fit_data <- data.frame(
#'   x = range,
#'   y = stats::pnorm(range, mean = pars$mu, sd = pars$sigma)
#' )
#' ggplot(data.frame(x = vec), aes(x = x)) +
#'   stat_ecdf(color = "blue") +
#'   stat_ecdf(data = data.frame(x = impute_outliers(vec)),
#'             aes(x = x), color = "purple") +
#'   geom_line(data = fit_data, aes(x = x, y = y),
#'             colour = "black", linetype = "longdash") +
#'   geom_vline(xintercept = pars$crit, colour = "red", linetype = "dashed") +
#'   ggtitle("Outlier Cutoffs (3*sigma) in Red")
#' @importFrom stats pnorm qnorm
#' @importFrom helpr get_outliers
#' @export
impute_outliers <- function(x, n_sigma = 3) {

  if ( length(table(x)) < 5L ) {   # catch for non-continuous data
    return(x)
  }

  # parameters stored in attributes
  idx   <- get_outliers(x, n_sigma = n_sigma, type = "parametric")
  pars  <- attributes(idx)
  crit  <- pars$crit
  mu    <- pars$mu
  sigma <- pars$sigma

  # convert to percentiles (CDF y-axis)
  pctiles <- rank(x, ties.method = "average") / length(x)

  # max value must be < 1.0 (set to the crit value)
  pctiles[which(pctiles == max(pctiles))] <- pnorm(crit[2L],
                                                   mean = mu,
                                                   sd   = sigma)
  # min value set to the crit value
  pctiles[which(pctiles == min(pctiles))] <- pnorm(crit[1L],
                                                   mean = mu,
                                                   sd   = sigma)
  # replace with Gaussian estimates
  x[idx] <- stats::qnorm(pctiles[ idx ], mean = mu, sd = sigma)
  x
}
