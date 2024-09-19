#' Revert and Convert Aptamer Names
#'
#' Legacy `AptNames`, features in ADATs, typically are
#' formatted by concatenating `EntrezGeneSymbols` with
#' `SeqIds`. This convention  changed to `seq.1234.56` to increase consistency
#' as the `GeneID` database changes. To enable legacy code to
#' run, this function enables you to reconstruct the legacy
#' `GeneID-AptName` format of a `soma_adat` object.
#' For example: `seq.1224.23` -> `ABCD.1234.23`
#' Please also see the `vignette("GeneIDs-AptNames")` for a more
#' comprehensive discussion on the topic.
#'
#' As a rough approximation, [revertAptNames()] attempts to reconstruct
#' the `GeneIDs` from the *existing* annotations in `x`, i.e. by a
#' call to [SomaDataIO::getAnalyteInfo()], and then using that information,
#' if systematically matches `SeqIds` in the names of `x` with the `SeqIds`
#' in the annotations table. It is important to not that these annotations
#' (the `Col.Meta` attribute) represents a snapshot in time, the information
#' available in the database *when the ADAT was created*. Therefore, GeneIDs
#' can change as the information in the database and our understanding
#' about reagent-target binding changes. As a result, [revertAptNames()]
#' may be imperfect if the contemporary annotations in the `Col.Meta` differ
#' from the snapshot in the past you wish to revert to.
#'
#' @param x For [revertAptNames()], either a `soma_adat` object or a
#'   character string of `Anonymous-AptNames` (i.e. `seq.1234.23` format).
#'   For [convertAptNames()], either a `soma_adat` or character string in
#'    the *old* `GeneID-AptName` format.
#' @param ... For extensibility to downstream methods.
#' @author Stu Field
#' @return [revertAptNames()]: object corresponding to type `x` with
#'   the *old* `ABCD.XXXX.XX` format (containing `GeneId` symbols).
#' @examples
#' adat <- splyr:::sample.adat
#' adat[, 20:30L]
#'
#' # go back to old format
#' adat <- revertAptNames(adat)
#' is_reverted(adat)
#'
#' adat[, 20:30L]
#'
#' # convert back to new format
#' adat <- convertAptNames(adat)
#' adat[, 20:30L]
#'
#' # revert also works on character class
#' # but you must pass a `tbl` of annotations
#' apts <- names(adat)[20:30L]
#' revertAptNames(
#'   apts,
#'   tbl = splyr:::apt_data
#' )
#'
#' # non-seqId characters are not mapped backward
#' revertAptNames(c("foo", "seq.2211.9", "bar"), splyr:::apt_data)
#'
#' # non-seqId characters are not mapped forward
#' convertAptNames(c("foo", "ABCD.2211.9", "bar", "MMP12.9898.3"))
#' @export
revertAptNames <- function(x, ...) UseMethod("revertAptNames")


#' @importFrom globalr value
#' @export
revertAptNames.default <- function(x, ...) {
  stop(
    "Couldn't find a S3 method for this class object: ", value(class(x)),
    call. = FALSE
  )
}


#' S3 method for 'soma_adat' class
#'
#' @param tbl An annotations table (e.g. `tibble`), containing `SeqId`
#'   information and `EntrezGeneSymbol` information. This is typically
#'   a call to [SomaDataIO::getAnalyteInfo()], but could be any annotations
#'   object. Most importantly, annotations are time-specific, and therefore
#'   this table must be *from the snapshot in time you wish to revert to*.
#'   Sometimes this may be an original training data set, or even an
#'   annotations table close to the desired time point.
#' @importFrom SomaDataIO regexSeqId
#' @rdname revertAptNames
#' @export
revertAptNames.soma_adat <- function(x, tbl = NULL, ...) {

  tbl <- tbl %||% attributes(x)$Col.Meta

  stopifnot(
    "`Type` must be in Col.Meta." = "Type" %in% names(tbl)
  )

  mapped_names <- revertAptNames(names(x), tbl = tbl)

  # safety; check that only SeqId names have been modified
  stopifnot(
    identical(
      grep(regexSeqId(), names(x)),
      grep(regexSeqId(), mapped_names)
    )
  )

  names(x) <- mapped_names
  structure(x, reverted_adat = TRUE)
}


#' S3 method for 'character' class
#'
#' @rdname revertAptNames
#' @importFrom globalr value
#' @export
revertAptNames.character <- function(x, tbl, ...) {
  # left_join ensures same order as orig 'x' (perhaps overkill)
  map <- dplyr::left_join(
    data.frame(old_value = x, seqid = getSeqId(x, TRUE)),
    map_geneIDs(tbl),
    by = "seqid"
  )
  map$new_value <- paste0(map$prefix, ".", sub("-", ".", map$seqid))
  map$no_map    <- !is.na(map$seqid) & is.na(map$prefix) # seqIds with no geneId

  if ( any(map$no_map) ) {
    warning(
      "There were ", value(sum(map$no_map)),
      " SeqIds that could not be mapped.", call. = FALSE
    )
  }
  ifelse(is.na(map$seqid), map$old_value, map$new_value)
}


#' Test for Reverted ADAT objects
#'
#' [is_reverted()] checks whether an object
#' has been reverted via [revertAptNames()].
#'
#' @rdname revertAptNames
#' @return [is_reverted()]: logical. Whether `x` is a `soma_adat`
#'   object that has been reverted.
#' @export
is_reverted <- function(x) {
  !is.null(attr(x, "reverted_adat")) &&
    isTRUE(attr(x, "reverted_adat"))
}


#' @importFrom SomaDataIO cleanNames getSeqId
#' @noRd
map_geneIDs <- function(tbl) {
  stopifnot(
    "`tbl` must be a data frame or tbl_df." = inherits(tbl, "data.frame"),
    "`EntrezGeneSymbol` must be in `tbl`."  = "EntrezGeneSymbol" %in% names(tbl),
    "`Type` must be in `tbl`."              = "Type" %in% names(tbl),
    "`Target` must be in `tbl`."            = "Target" %in% names(tbl)
  )
  # mappings
  map <- c("spuriomer"     = "Spuriomer",
           "deprecated"    = "Deprecated",
           "non-biotin"    = "NonBiotin",
           "non-human"     = "NonHuman",
           "non-cleavable" = "NonCleavable",
           "hybridization-control-elution" = "HybControlElution")

  tbl$seqid <- getSeqId(tbl$SeqId, TRUE)         # for safety; old adats
  tbl$type  <- gsub(" ", "-", tolower(tbl$Type)) # catch random DB Caps/Spaces

  tbl$prefix <- unname(map[tbl$type]) # map specials; no-match is NA
  # otherwise use EntrezGeneSymbol
  tbl$prefix <- ifelse(is.na(tbl$prefix), tbl$EntrezGeneSymbol, tbl$prefix)
  # special case for Fc_MOUSE; use Target
  tbl$prefix <- ifelse(tbl$Target == "Fc_MOUSE", "Fc_MOUSE", tbl$prefix)
  tbl$prefix <- cleanNames(tbl$prefix)
  tbl[, c("seqid", "prefix")]
}


#' Convert Old AptNames to New Format
#'
#' [convertAptNames()] renames an ADAT with the *old* `GeneID-AptName` format
#'   to the new "`Anonymous-AptName`" format.
#'   For example: `ABCD.1234.23` -> `seq.1224.23`.
#'
#' @rdname revertAptNames
#' @return [convertAptNames()]: object corresponding to type `x` with
#'   the *new* `seq.XXXX.XX` format (i.e. with a `seq.` prefix).
#' @export
convertAptNames <- function(x) UseMethod("convertAptNames")

#' @importFrom globalr value
#' @export
convertAptNames.default <- function(x) {
  stop(
    "Couldn't find a S3 method for this class object: ", value(class(x)),
    call. = FALSE
  )
}

#' @noRd
#' @importFrom SomaDataIO seqid2apt getSeqId
#' @export
convertAptNames.character <- function(x) {
  map <- getSeqId(x, TRUE)
  lgl <- is.na(map)    # these elements do NOT contain SeqIds
  map[!lgl] <- seqid2apt(map[!lgl])
  map[lgl]  <- x[lgl]
  map
}

#' @noRd
#' @importFrom SomaDataIO is_seqFormat getAnalytes
#' @importFrom globalr signal_done
#' @export
convertAptNames.soma_adat <- function(x) {
  if ( is_seqFormat(x) || getAnalytes(x, n = TRUE) == 0L ) {
    signal_done("Object is already in the new `seq.XXXX.XX` format (or no SeqIds).")
  } else {
    names(x) <- convertAptNames(names(x))
  }
  x
}
