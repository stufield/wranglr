#' Create Child Attributes
#'
#' This function re-creates the corrupted attribute list of a
#' modified `soma_adat` given original parent attributes of
#' the data frame used to create the modified child. With extensive
#' S3 methods for the `soma_adat` class, this *should* happen only rarely,
#' but can happen when removing columns from clinical or feature data,
#' or when completely re-constructing a `soma_adat` from scratch. A
#' call to [is_intact_attr()] should tell you if the
#' attributes need to be re-created.
#'
#' @param child.df The `soma_adat` (`data.frame` object) that
#'   potentially has corrupted/broken attributes.
#' @param parent The parent data frame object that was
#'   used to create the child data frame.
#' @param verbose Logical. Should the checks and output be printed
#'   to the console? These checks can be quite extensive, so by default,
#'   this only occurs if called in an interactive session. See [interactive()].
#' @return The object corresponding to `child.df` with an
#'   *updated* list of attributes containing the following:
#' \item{names}{The column/feature names.}
#' \item{row.names}{The row names of the data frame.}
#' \item{class}{The class(es) of the object, typically
#'              `c("soma_adat", "data.frame")`.}
#' \item{Header.Meta}{A list itself containing all the
#'                    header meta data of the original adat.}
#' \item{Col.Meta}{A tibble containing all the column/feature
#'                 meta data of the original adat.}
#' \item{file_specs}{A list of the original file specs during file parsing.}
#' \item{row_meta}{A vector of the meta data.}
#' @author Stu Field
#' @seealso [is_intact_attr()], [inner_join()]
#' @examples
#' # The "parent" data frame
#' adat <- sim_test_data
#'
#' feats <- withr::with_seed(1, sample(names(adat), 10L))  # random set of features
#' new_adat <- adat[1:10L, c(head(SomaDataIO::getMeta(adat), 10L), feats) ]
#' dim(new_adat)
#'
#' dim(adat)
#'
#' SomaDataIO::is_intact_attr(new_adat)           # TRUE
#'
#' new <- createChildAttributes(new_adat, adat)   # Congrats!
#'
#' # now really break the attributes & reconstruct them
#' attributes(new_adat)$Header.Meta <- NULL
#'
#' SomaDataIO::is_intact_attr(new_adat, verbose = TRUE)  # FALSE
#' new_adat$foo <- "bar"                     # add a clin variable
#'
#' new_adat <- createChildAttributes(new_adat, adat)
#'
#' SomaDataIO::is_intact_attr(new_adat)   # TRUE; atts are back!
#' @importFrom globalr diff_vecs pad signal_rule value add_color
#' @importFrom globalr signal_done signal_info signal_todo
#' @importFrom SomaDataIO getAnalytes is_intact_attr getSeqId matchSeqIds addAttributes
#' @importFrom tibble tibble
#' @export
createChildAttributes <- function(child.df, parent, verbose = interactive()) {

  if ( is_intact_attr(child.df, verbose = FALSE) ) {
    if ( verbose ) {
      signal_done(
        "Congrats! Looks like `child.df` has GOOD attributes. You're good to go!"
      )
    }
    return(child.df)
  }

  stopifnot(inherits(parent, "soma_adat"))
  child_apts  <- getAnalytes(child.df)
  child_atts  <- attributes(child.df)
  parent_atts <- attributes(parent)

  if ( verbose ) {
    writeLines(
      signal_rule("Reconstructing broken child attributes", line_col = "blue")
    )
  }

  getType <- function(.x) {
    vapply(.x, typeof, USE.NAMES = FALSE, FUN.VALUE = "")
  }

  # fix header meta ----
  meta_names <- getMeta(child.df)
  rowTypes   <- getType(child.df[, meta_names, drop = FALSE])
  # print(rowTypes)   # nolint: commented_code_linter.

  child_atts$Header.Meta               <- parent_atts$Header.Meta
  child_atts$Header.Meta$ROW_DATA$Name <- meta_names  # ensures atts match data frame
  child_atts$Header.Meta$ROW_DATA$Type <- rowTypes

  # fix col meta ----
  parent_Col.Meta <- dplyr::ungroup(parent_atts$Col.Meta) |>
    mutate(seqid = getSeqId(SeqId, TRUE)) # add lookup key w/o version

  parent_apts_n <- nrow(parent_Col.Meta)  # original no. features in parent

  if ( parent_apts_n < 1L ) {
    stop("The parent `Col.Meta` has no rows!", call. = FALSE)
  }

  # subset parent Col.Meta based on child features

  # This is the key moment! If fails, this is where it will go wrong
  # `inner_join()` here b/c orders by `x`; but `x` must all be unique
  stopifnot(length(child_apts) == length(unique(child_apts)))
  new_colmeta <- dplyr::inner_join(
    tibble(seqid = getSeqId(child_apts, TRUE)),
    parent_Col.Meta, by = "seqid"
  )

  # catch for 0-length tibble in Col.Meta
  stopifnot(nrow(new_colmeta) > 0L)

  # check for orphaned SeqIds in child
  orph <- setdiff(getSeqId(child_apts, TRUE),
                  matchSeqIds(child_apts, new_colmeta$seqid))

  if ( length(orph) != 0L ) {
    stop(
      "There are SeqIds in `child.df` *not* in parent. ",
       "Cannot create attributes from this parent.", call. = FALSE
    )
  }

  # don't need key now
  child_atts$Col.Meta <- dplyr::select(new_colmeta, -seqid)

  # ensure header meta matches col meta
  child_atts$Header.Meta$COL_DATA$Name <- names(child_atts$Col.Meta)
  child_atts$Header.Meta$COL_DATA$Type <- getType(child_atts$Col.Meta)

  # -------------------- #
  # Long verbose section ----
  # (for debugging)
  # -------------------- #
  if ( verbose ) {
    # nolint start
    pad <- 27
    X <- parent_atts$Header.Meta$ROW_DATA
    msg  <- pad("Feature(s) removed", width = pad)
    signal_info(msg, value(parent_apts_n - length(child_apts)))
    msg  <- pad("Meta data field(s) removed", width = pad)
    signal_info(msg, value(length(setdiff(X$Name, getMeta(child.df)))))

    rm_meta <- setdiff(X$Name, getMeta(child.df))
    signal_todo(" ", value(rm_meta))

    new_meta <- setdiff(getMeta(child.df), X$Name)
    msg <- pad("Meta data field(s) added", width = pad)
    signal_info(msg, value(length(new_meta)))
    signal_todo(" ", value(new_meta))

    msg <- pad("Sample(s) removed", width = pad)
    signal_info(
      msg,
      value(length(parent_atts$row.names) - length(child_atts$row.names))
    )

    cat("\n")
    writeLines(
      signal_rule("Output Child Attributes Checks", line_col = "blue")
    )

    logic_colmeta <- isTRUE(all.equal(child_atts$Header.Meta$COL_DATA$Name,
                                      names(child_atts$Col.Meta)))

    msg <- "Header.Meta$COL_DATA == child col meta data fields"
    signal_done(
      msg,
      sprintf("(%s)", add_color(logic_colmeta, ifelse(logic_colmeta, "green", "red")))
    )

    if ( !logic_colmeta ) {
      writeLines(signal_rule("They differ in:", line_col = "blue"))
      tmp_diff <- diff_vecs(child_atts$Header.Meta$COL_DATA$Name,
                            names(child_atts$Col.Meta), verbose = FALSE)
      first  <- tmp_diff[[1L]]
      second <- tmp_diff[[2L]]
      signal_todo("  In COL_DATA not Col.Meta:", value(first))
      signal_todo("  In Col.Meta not COL_DATA:", value(second))
    }

    logic_rowmeta <- identical(child_atts$Header.Meta$ROW_DATA$Name,
                               getMeta(child.df))

    msg <- "Header.Meta$ROW_DATA == child row meta data fields"
    signal_done(
      msg,
      sprintf("(%s)", add_color(logic_rowmeta, ifelse(logic_rowmeta, "green", "red")))
    )

    if ( !logic_rowmeta ) {
      writeLines(signal_rule("They differ in:", line_col = "blue"))
      tmp_diff <- diff_vecs(child_atts$Header.Meta$ROW_DATA$Name,
                              getMeta(child.df), verbose = FALSE)
      first  <- tmp_diff[[1L]]
      second <- tmp_diff[[2L]]
      signal_todo("  In ROW_DATA not MetaData:", value(first))
      signal_todo("  In MetaData not ROW_DATA:", value(second))
    }
    writeLines(signal_rule(line_col = "green", lty = "double"))
    # nolint end
  }
  attributes(child.df) <- child_atts
  addAttributes(child.df, parent_atts)
}
