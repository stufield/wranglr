#' Simulated Test Data
#'
#' Simulates SomaScan data by randomly generating data for various
#' Bioinformatics `POC` related analyses. Feature names will be randomly
#' generated to have "*SOMAmer-like*" names so that downstream tools will
#' function seamlessly. Model types this data can support:
#'   * Binary classification
#'   * Continuous regression
#'   * Survival
#' @format The format of `sim_test_data` (100 x 47) is:
#' \describe{
#' \item{id}{A sample `id` number.}
#' \item{class_response}{The classification response variable.}
#' \item{reg_response}{The regression response variable.}
#' \item{time}{The time-to-event variable for survival problems.}
#' \item{status}{The event status variable for survival problems.}
#' \item{SampleId}{Identical to the `id` column; for `POC` tools to run.}
#' \item{SlideId}{Randomly generated numeric, used to create rownames.}
#' \item{Subarray}{All `1`s. Used to create rownames.}
#' \item{SiteId}{Locations from `LOTR`; for `POC` tools to run.}
#' \item{age}{A randomly assigned age variable: 18 - 80.}
#' \item{gender}{A randomly assigned gender variable: "F" or "M".}
#' \item{HybControlNormScale}{Random continuous data in \verb{[0.4, 2.5]}.}
#' \item{NormScale_40}{Random continuous data in \verb{[0.4, 2.5]}.}
#' \item{NormScale_0_005}{Random continuous data in \verb{[0.4, 2.5]}.}
#' \item{NormScale_1}{Random continuous data in \verb{[0.4, 2.5]}.}
#' }
#'
#' General features of the data (see `attributes(sim_test_data)`):
#'   * 5 significant classification analytes
#'   * 5 significant regression analytes
#'   * 5 significant survival analytes
#'   * 25 noise analytes
#'   * 4 sex/gender analytes (with noise features)
#'   * `PGAM` (`seq.3896.5` for sample handling
#' @author Stu Field, Leigh Alexander
#' @examples
#' sim_test_data
"sim_test_data"
