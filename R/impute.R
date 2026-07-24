#' Impute Missing Values
#'
#' There are two imputation strategies:
#'
#' @name impute
#' @family impute
#'
#' @param x A `data.frame`, matrix, or numeric vector. Factors or
#'   character strings are also possible but see caveat above.
#'
#' @return An object of the same dimension and class as `x`
#'   with any `NA` (missing) values replaced.
#'
#' @author Stu Field
#' @seealso [median()]
#'
#' @examples
#' # data frame method
#' df <- select(mtcars, mpg, cyl, disp) |> head()
#' df[cbind(2:3, 2:3)] <- NA_real_
#' df
#'
#' impute_median(df)
NULL


#' Impute Median
#'
#' Imputing with the median is typically performed for
#'   numeric vectors, however methods also exist for:
#'   * the `factor` method imputes to the most common factor level.
#'   * the `character` method imputes to the most common string
#'
#' @rdname impute
#' @export
impute_median <- function(x) UseMethod("impute_median")

#' @noRd
#' @importFrom helpr value
#' @export
impute_median.default <- function(x) {
  stop(
    "Couldn't find a S3 method for this class object: ",
    value(class(x)), call. = FALSE
  )
}

#' @noRd
#' @export
impute_median.character <- function(x) {
  nas <- which(is.na(x))
  if ( length(nas) == 0L ) {
    return(x)
  }
  repl <- sample(na.omit(unique(x)), length(nas), prob = prop.table(table(x)))
  x[nas] <- repl
  x
}

#' @noRd
#' @export
impute_median.factor <- function(x) {
  out <- impute_median(as.character(x))
  factor(out, levels = levels(x))
}

#' @noRd
#' @export
impute_median.data.frame <- function(x) {
  atts <- attributes(x)
  # function to get only cols with NAs & numeric
  .nas <- function(x) is.numeric(x) & any(is.na(x))
  new <- .modify_if(x, .nas, impute_median.numeric)
  attributes(new) <- atts
  new
}

#' @noRd
#' @export
impute_median.matrix <- function(x) {
  apply(x, 2, impute_median)
}

#' @noRd
#' @importFrom stats median
#' @export
impute_median.numeric <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}

.modify_if <- function(x, p, f) {
  lgl <- vapply(x, p, NA)
  for ( i in which(lgl) ) {
    x[[i]] <- f(x[[i]])
  }
  x
}



#' Impute From Distribution
#'
#' Imputing missing values from an estimated distribution is
#'   performed by fitting via Maximum Likelihood and estimating
#'   and parameters, followed by random draws from the appropriate
#'   random generator.
#'   Currently only supports Gamma and Gaussian distributions.
#'
#' @rdname impute
#'
#' @param distr `character(1)`. The name of the desired distribution.
#'
#' @param seed `integer(1)`. Set a seed for reproducibility.
#'
#' @examples
#' x <- rnorm(10, mean = 10, sd = 5)
#' x <- rbeta(10, shape1 = 2, shape2 = 3)
#' x[c(2, 7)] <- NA_real_
#'
#' data.frame(
#'   orig    = x,
#'   imputed = impute_missing(x)   # gaussian
#' )
#' @export
impute_missing <- function(x, distr = c("norm", "gamma", "beta"),
                           seed = 123) {
  UseMethod("impute_missing")
}

#' @noRd
#' @importFrom helpr value
#' @export
impute_missing.default <- function(x, distr, seed) {
  stop(
    "Couldn't find a S3 method for this class object: ",
    value(class(x)), call. = FALSE
  )
}

#' @noRd
#' @export
impute_missing.numeric <- function(x, distr = "norm", seed = 123) {
  fit   <- switch(distr,
    gamma = fit_gamma_mle(x),
    norm  = fit_norm_mle(x),
    beta  = fit_beta_mle(x),
    stop("`distr` should be one of: ",
         value(c("norm", "gamma", "beta")), ".", call. = FALSE)
  )
  rfn  <- switch(distr,
                 gamma = stats::rgamma,
                 beta  = stats::rbeta,
                 norm  = stats::rnorm)
  repl_lgl <- is.na(x)
  args <- c(n = sum(repl_lgl), .get_dist_params(fit, distr))
  x[repl_lgl] <- withr::with_seed(seed, do.call(rfn, args))
  x
}


#' Helper pulls appropriate named pars
#'  from distr function to pass to `do.call()`
#'
#' @noRd
.get_dist_params <- function(fit, distr) {
  switch(distr,
    norm  = as.list(fit[c("mean", "sd")]),
    beta  = as.list(fit[c("shape1", "shape2")]),
    gamma = as.list(fit[c("shape", "rate")])
  )
}

.prep_fit <- function(x) {
  stopifnot(length(x) >= 10)
  x[!is.na(x)]
}

#' @importFrom stats dgamma optim var
#' @noRd
fit_gamma_mle <- function(x) {
  x <- .prep_fit(x)
  x[x == 0] <- 1e-04   # trim extreme values for fit
  # Method-of-moments starting values (closed form for gamma)
  #   mean = shape / rate  →  shape = mean^2 / var
  #   var  = shape / rate² →  rate  = mean / var
  mu <- mean(x)
  s2 <- var(x)
  start <- c(shape = mu^2 / s2, rate = mu / s2)

  nloglik <- function(pars, data) { # negative log-likelihood
    -sum(dgamma(x = data, shape = pars["shape"],
                rate = pars["rate"], log = TRUE))
  }

  fit <- optim(par = start, fn = nloglik, data = x,
               method = "L-BFGS-B",
               lower = c(shape = 1e-06, rate = 1e-06))
  tibble(
    method    = "MaxLik",
    estimator = "-logLikelihood",
    estimate  = -fit$value,
    shape     = fit$par[["shape"]],
    rate      = fit$par[["rate"]],
    scale     = 1 / rate,  # nolint: object_usage_linter.
    converged = fit$convergence == 0
  )
}

#' @importFrom stats dnorm optim sd
#' @noRd
fit_norm_mle <- function(x) {
  x <- .prep_fit(x)
  start <- c(mean = mean(x), sd = sd(x))
  nloglik <- function(pars, data) { # negative log-likelihood
    -sum(dnorm(x = data, mean = pars["mean"],
               sd = pars["sd"], log = TRUE))
  }
  fit <- optim(par = start, fn = nloglik, data = x,
               method = "L-BFGS-B",
               lower = c(mean = 1e-06, sd = 1e-06))
  tibble(
    method    = "MaxLik",
    estimator = "-logLikelihood",
    estimate  = -fit$value,
    mean      = fit$par[["mean"]],
    sd        = fit$par[["sd"]],
    converged = fit$convergence == 0
  )
}

#' @importFrom stats dbeta optim var
#' @noRd
fit_beta_mle <- function(x) {
  x  <- .prep_fit(x)
  x[x == 0] <- 1e-04   # trim extreme values for fit
  x[x == 1] <- 1 - 1e-4
  mu <- mean(x)
  s2 <- var(x)
  common <- (mu * (1 - mu) / s2) - 1
  # Same method of moments (MoM) as gamma
  start <- c(shape1 = mu * common, shape2 = (1 - mu) * common)
  nloglik <- function(pars, data) { # negative log-likelihood
    -sum(dbeta(x = data, shape1 = pars["shape1"],
               shape2 = pars["shape2"], log = TRUE))
  }
  fit <- optim(par = start, fn = nloglik, data = x,
               method = "L-BFGS-B",
               lower = c(shape1 = 1e-06, shape2 = 1e-06))
  tibble(
    method    = "MaxLik",
    estimator = "-logLikelihood",
    estimate  = -fit$value,
    shape1    = fit$par[["shape1"]],
    shape2    = fit$par[["shape2"]],
    converged = fit$convergence == 0
  )
}
