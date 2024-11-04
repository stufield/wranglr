#' Get Graph Database Annotations
#'
#' Generate a lookup table (tibble) of annotations
#'   from the Graph Database keyed on `SeqId`.
#'   By default, an internal static object
#'   corresponding to analyte annotations is returned.
#'   Alternatively, a request is sent to the
#'   internal Graph DB API endpoint to pull a
#'   live/dynamic annotations table via a call
#'   to [getAnnotationsAPI()].
#'
#' @family Annotations
#' @param api Logical. Should the API endpoint be queried, if
#'   available, for live updated annotation information?
#' @param path The umbrella API endpoint address. Ignored if `api = FALSE`.
#' @return A tibble of the corresponding column meta data available in
#'   the Annotations Graph DB.
#' @examples
#' getAnnotations()
#' @author Stu Field
#' @importFrom helpr signal_oops value
#' @importFrom tibble as_tibble
#' @export
getAnnotations <- function(api = FALSE, path = "annotations_graph/api") {
  if ( api ) {
    getAnnotationsAPI(path = path)
  } else {
    annotations_v5.0
  }
}

#' @describeIn getAnnotations
#'   Similar to [getAnnotations()] but requests
#'   live dynamic annotations from the Graph DB API.
#' @importFrom purrr transpose
#' @importFrom tidyr unnest
#' @export
getAnnotationsAPI <- function(path = "annotations_graph/api") {
  url  <- httr::modify_url("https://umbrella.somalogic.io", path = path)
  resp <- tryCatch(httr::GET(url, httr::timeout(3)),
                   error = function(e) NULL)

  if ( is.null(resp) ) {
    signal_oops("Could not connect to", value(url),
                "... using static internal object.")
    return(getAnnotations(api = FALSE))
  }

  if ( httr::status_code(resp) != 200 ) {
    httr::warn_for_status(resp)
    return(getAnnotations(api = FALSE))
  }

  tbl <- httr::content(resp, "parsed", encoding = "UTF-8")$somareagents |>
    purrr::transpose() |>
    as_tibble()
  unnest(tbl, cols = names(tbl)) |>
    select(-EnsemblGeneId, -HgncId) |>
    mutate(Feature = add_seq(SeqId)) |>
    select(SeqId, Feature, everything())
}
