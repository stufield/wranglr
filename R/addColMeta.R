#' Add Annotations to Col Meta
#'
#' Uses modified analyte info of annotations
#' generated using [getAnalyteInfo()] and adds it to the
#' `Col.Meta` attribute of a `soma_adat`
#' (see `attributes(adat)$Col.Meta`).
#' The column names of the ADAT are *not* conserved.
#' This could be considered the inverse of [getAnalyteInfo()].
#'
#' @param adat A `soma_adat` object with intact attributes,
#'   specifically the `Col.Meta` element.
#' @param apt.data A modified "annotations"
#'   `tibble` object to add to attributes.
#' @return A `soma_adat` object with new attributes and `Col.Meta`
#'   corresponding to the *additional* columns of `apt.data`
#'   not present in the existing ADAT.
#'   Rows *not* present in `apt.data` will contain an `NA` in
#'   the resulting attributes.
#' @author Stu Field, Eduardo Tabacman
#' @seealso [dplyr::left_join()], [getAnalyteInfo()], [getSeqId()]
#' @examples
#' adat <- sim_test_data
#' ad <- SomaDataIO::getAnalyteInfo(adat)
#' ad
#'
#' # Adds 2 new 'random' variables:
#' ad$random1 <- round(rnorm(nrow(ad)), 2)
#' ad$random2 <- round(rnorm(nrow(ad)), 2)
#' new <- addColMeta(adat, ad)
#' SomaDataIO::getAnalyteInfo(new) |> select(AptName, SeqId, starts_with("random"))
#' @importFrom SomaDataIO getAnalytes getAnalyteInfo is_intact_attr getSeqId
#' @export
addColMeta <- function(adat, apt.data) {

  stopifnot("`adat` must have intact attributes." = is_intact_attr(adat))

  if ( nrow(apt.data) != getAnalytes(adat, n = TRUE) ) {
    stop(
      "Error in dimensions to be added via `addColMeta()`. ",
      "Different number of rows in `apt.data` vs. aptamers in `adat`.",
      call. = FALSE
    )
  }

  colmeta <- ungroup(attr(adat, "Col.Meta")) |> # safety; old use grouped
    mutate(SeqId = getSeqId(SeqId, TRUE))       # create key; no version

  new_vars <- setdiff(names(apt.data), c("AptName", names(colmeta)))
  newdata  <- ungroup(apt.data)   # ungroup for safety (old code)
  newdata$SeqId <- getSeqId(newdata$SeqId, TRUE) # create key; no version
  newdata  <- newdata[, c("SeqId", new_vars)]  # only "new" cols

  new_colmeta <- left_join(colmeta, newdata, by = "SeqId")
  structure(adat, Col.Meta = new_colmeta)
}
