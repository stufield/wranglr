#' Calculate CV Decomposition
#'
#' Calculate the appropriate within and between assay
#'   run variance decomposition prior to calculating
#'   the %CV for a reproducibility study assay run.
#'
#' @param x A list of vectors, each representing a separate assay run
#'   for a particular analyte. The entries of the vectors should be replicates
#'   of intra-run assay variation. **Important!**: be sure to remove any `NAs`,
#'   if present, beforehand.
#'
#' @return A named vector of the CV decomposition for the replicates.
#' @author Stu Field, Dom Zichi, Eduardo Tabacman
#'
#' @seealso [calc_ss()]
#'
#' @references Calculation follows the method laid out
#'   by Dom Zichi, _Decomposition of Assay Variance_, 13 August 2011.
#'
#' @examples
#' # Example 1 (Dom's example)
#' dom_example <- list(A = c(5785.1, 5005.6, 5686.3, 5990.8,
#'                           5235.4, 5340.6, 5272.6, 5905.2),
#'                     B = c(5708.0, 5300.7, 5339.8, 5393.0,
#'                           5762.0, 5553.4, 6081.4, 5473.5),
#'                     C = c(5409.4, 5353.6, 5398.0, 5631.2,
#'                           5646.1, 5073.4, 5879.2, 5617.5))
#' dom_example
#' calc_cv(dom_example)
#'
#' # Example 2 (dummy `simdata` example)
#' # look at how the intra, inter, and total variance.
#' # We assume `simdata` contains *only* replicate samples
#' # and assume the `SiteId` represents assay "runs".
#' # We can then `lapply()` over each feature, calculating
#' # the `CVdecomp` for each and store to a data frame
#' split_on <- simdata$SiteId
#'
#' # split data
#' site_split <- feature_matrix(simdata) |> data.frame() |>
#'   lapply(split, f = split_on)
#'
#' # iterate over list/analytes; each contains multiple (4x) "runs"
#' coeffVar <- lapply(site_split, calc_cv)
#' coeffVar <- do.call(rbind, coeffVar) |> data.frame()
#' @export
calc_cv <- function(x) {
  if ( inherits(x, "data.frame") ) {
    stop("Please recast data.frame `x` as a list of vectors.",
         call. = FALSE)
  }
  all_x <- unlist(x)     # all values un-grouped
  if ( any(is.na(all_x)) ) {
    stop("`NAs` detected in some entries. Please remove and retry.",
         call. = FALSE)
  }
  group_n  <- lengths(x, use.names = FALSE)  # vector of n
  sum_n1   <- sum(group_n) - 1               # denominator below
  group_mu <- vapply(x, mean, double(1))     # groupwise means
  grand_mu <- mean(all_x)                    # grand mean
  SSinter  <- sum(group_n * (group_mu - grand_mu)^2)
  SStotal  <- sum((all_x - grand_mu)^2)
  SSintra  <- SStotal - SSinter
  sqrt(c(Intra = SSintra, Inter = SSinter, Total = SStotal) / sum_n1) / grand_mu
}
