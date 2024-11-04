
# dummy data.frame -> soma_adat
dress_adat <- function(df) {
  stopifnot(inherits(df, "data.frame"))
  row_meta <- get_meta(df)
  structure(
    df,
    class = c("soma_adat", "data.frame"),
    Header.Meta = list(HEADER   = list(Version = "1.2", Title = "SL-99-999"),
                       COL_DATA = list(Name = c("SeqId", "Target", "Dilution", "Units"),
                                       Type = rep_len("String", 4L)),
                       ROW_DATA = list(Name = row_meta,
                                       Type = rep_len("String", length(row_meta)))),
    Col.Meta = tibble::tibble(SeqId    = c("1234-56", "9999-88"),
                              Target   = c("Gandalf-4", "Frodo-5"),
                              Dilution = c("1", "0.005"),
                              Units    = c("RFU", "RFU")),
    file_specs = list(empty_adat     = FALSE,
                      table_begin    = 10,
                      col_meta_start = 11,
                      col_meta_shift = 15,
                      data_begin     = 11 + 4,
                      old_adat       = FALSE),
    row_meta = row_meta
  )
}
