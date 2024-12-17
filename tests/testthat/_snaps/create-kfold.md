# `.make_strata()` returns expected results and warnings

    Code
      out <- .make_strata(x, breaks = 10, depth = 50L)
    Condition
      Warning:
      The number of observations in each quantile is below the recommended threshold of 50.
      Stratification will be done with 2 breaks instead.
      To override this limit, provide `depth` as input.

---

    Code
      out <- .make_strata(x, breaks = 10, depth = 70L)
    Condition
      Warning:
      The number of observations in each quantile is below the recommended threshold of 70.
      Stratification will be done with 1 breaks instead.
      To override this limit, provide `depth` as input.
      Warning:
      Too little data to stratify. Non-stratified resampling will be used.

# `create_kfold()` returns expected results for repeats = 1

    Code
      out <- withr::with_seed(234L, create_kfold(simdata, breaks = list(time = 4L),
      depth = 70L))
    Condition
      Warning:
      The number of observations in each quantile is below the recommended threshold of 70.
      Stratification will be done with 1 breaks instead.
      To override this limit, provide `depth` as input.
      Warning:
      Too little data to stratify. Non-stratified resampling will be used.

# `S3 print()` returns expected class `x_split`

    Code
      create_kfold(simdata, k = 4L, repeats = 1L)
    Output
      == A `x_split` object ==========================================================
      • k            4
      • repeats      1
      • stratified   'NULL'
      • orig data    simdata
      -- split info ------------------------------------------------------------------
      # A tibble: 4 x 3
        split             Fold Repeat
        <list>           <int>  <int>
      1 <named list [2]>     1     NA
      2 <named list [2]>     2     NA
      3 <named list [2]>     3     NA
      4 <named list [2]>     4     NA
      ================================================================================

---

    Code
      create_kfold(simdata, k = 4L, repeats = 3L)
    Output
      == A `x_split` object ==========================================================
      • k            4
      • repeats      3
      • stratified   'NULL'
      • orig data    simdata
      -- split info ------------------------------------------------------------------
      # A tibble: 12 x 3
         split             Fold Repeat
         <list>           <int>  <int>
       1 <named list [2]>     1      1
       2 <named list [2]>     2      1
       3 <named list [2]>     3      1
       4 <named list [2]>     4      1
       5 <named list [2]>     1      2
       6 <named list [2]>     2      2
       7 <named list [2]>     3      2
       8 <named list [2]>     4      2
       9 <named list [2]>     1      3
      10 <named list [2]>     2      3
      11 <named list [2]>     3      3
      12 <named list [2]>     4      3
      ================================================================================

---

    Code
      df <- as.data.frame(simdata[, c("time", "status")])
      create_kfold(df, k = 5L)
    Output
      == A `x_split` object ==========================================================
      • k            5
      • repeats      1
      • stratified   'NULL'
      • orig data    df
      -- split info ------------------------------------------------------------------
      # A tibble: 5 x 3
        split             Fold Repeat
        <list>           <int>  <int>
      1 <named list [2]>     1     NA
      2 <named list [2]>     2     NA
      3 <named list [2]>     3     NA
      4 <named list [2]>     4     NA
      5 <named list [2]>     5     NA
      ================================================================================

---

    Code
      create_kfold(simdata, k = 5L, breaks = list(status = NA))
    Output
      == A `x_split` object ==========================================================
      • k            5
      • repeats      1
      • stratified   'list(status = NA)'
      • orig data    simdata
      -- split info ------------------------------------------------------------------
      # A tibble: 5 x 3
        split             Fold Repeat
        <list>           <int>  <int>
      1 <named list [2]>     1     NA
      2 <named list [2]>     2     NA
      3 <named list [2]>     3     NA
      4 <named list [2]>     4     NA
      5 <named list [2]>     5     NA
      ================================================================================

---

    Code
      create_kfold(simdata, k = 5L, repeats = 3L, breaks = list(time = 4L, status = NA))
    Output
      == A `x_split` object ==========================================================
      • k            5
      • repeats      3
      • stratified   'list(time = 4L, status = NA)'
      • orig data    simdata
      -- split info ------------------------------------------------------------------
      # A tibble: 15 x 3
         split             Fold Repeat
         <list>           <int>  <int>
       1 <named list [2]>     1      1
       2 <named list [2]>     2      1
       3 <named list [2]>     3      1
       4 <named list [2]>     4      1
       5 <named list [2]>     5      1
       6 <named list [2]>     1      2
       7 <named list [2]>     2      2
       8 <named list [2]>     3      2
       9 <named list [2]>     4      2
      10 <named list [2]>     5      2
      11 <named list [2]>     1      3
      12 <named list [2]>     2      3
      13 <named list [2]>     3      3
      14 <named list [2]>     4      3
      15 <named list [2]>     5      3
      ================================================================================

