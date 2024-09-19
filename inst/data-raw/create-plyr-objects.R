#' ----------------------------------
#' Generate and save "Plyr" package
#' objects as `data/plyr.rda` compressed file.
#' @author Stu Field
#' ----------------------------------
#' run: make objects
#' ----------------------------------
# mLod tables
mLod <- list()
mLod$plasma <- utils::read.csv("inst/data-raw/csv/mLod_plasma.csv",
                               row.names = 1, stringsAsFactors = FALSE)
mLod$serum  <- utils::read.csv("inst/data-raw/csv/mLod_serum.csv",
                               row.names = 1, stringsAsFactors = FALSE)

# V4 Validation Table
v4Validation <- utils::read.csv(
  "inst/data-raw/csv/annotated_validation_metrics_full_20180821.csv",
  stringsAsFactors = FALSE
) |> tibble::as_tibble()

save(mLod, v4Validation, file = "data/plyr.rda", compress = "xz")
