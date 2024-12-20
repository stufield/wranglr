# `S3 print()` returns expected class `x_split`

    Code
      create_kfold(mtcars2, k = 4L, repeats = 1L)
    Output
      == A `x_split` object ==========================================================
      • k            4
      • repeats      1
      • stratified   'NULL'
      • orig data    mtcars2
      -- split info ------------------------------------------------------------------
      # A tibble: 4 x 3
        split             fold .repeat
        <list>           <int>   <int>
      1 <named list [2]>     1      NA
      2 <named list [2]>     2      NA
      3 <named list [2]>     3      NA
      4 <named list [2]>     4      NA
      ================================================================================

---

    Code
      create_kfold(mtcars2, k = 4L, repeats = 3L)
    Output
      == A `x_split` object ==========================================================
      • k            4
      • repeats      3
      • stratified   'NULL'
      • orig data    mtcars2
      -- split info ------------------------------------------------------------------
      # A tibble: 12 x 3
         split             fold .repeat
         <list>           <int>   <int>
       1 <named list [2]>     1       1
       2 <named list [2]>     2       1
       3 <named list [2]>     3       1
       4 <named list [2]>     4       1
       5 <named list [2]>     1       2
       6 <named list [2]>     2       2
       7 <named list [2]>     3       2
       8 <named list [2]>     4       2
       9 <named list [2]>     1       3
      10 <named list [2]>     2       3
      11 <named list [2]>     3       3
      12 <named list [2]>     4       3
      ================================================================================

---

    Code
      df <- data.frame(mtcars2[, c("disp", "vs")])
      create_kfold(df, k = 5L)
    Output
      == A `x_split` object ==========================================================
      • k            5
      • repeats      1
      • stratified   'NULL'
      • orig data    df
      -- split info ------------------------------------------------------------------
      # A tibble: 5 x 3
        split             fold .repeat
        <list>           <int>   <int>
      1 <named list [2]>     1      NA
      2 <named list [2]>     2      NA
      3 <named list [2]>     3      NA
      4 <named list [2]>     4      NA
      5 <named list [2]>     5      NA
      ================================================================================

---

    Code
      create_kfold(mtcars2, k = 5L, breaks = list(vs = NULL))
    Output
      == A `x_split` object ==========================================================
      • k            5
      • repeats      1
      • stratified   'list(vs = NULL)'
      • orig data    mtcars2
      -- split info ------------------------------------------------------------------
      # A tibble: 5 x 3
        split             fold .repeat
        <list>           <int>   <int>
      1 <named list [2]>     1      NA
      2 <named list [2]>     2      NA
      3 <named list [2]>     3      NA
      4 <named list [2]>     4      NA
      5 <named list [2]>     5      NA
      ================================================================================

---

    Code
      create_kfold(mtcars2, k = 5L, repeats = 3L, breaks = list(disp = 3L, vs = NULL))
    Output
      == A `x_split` object ==========================================================
      • k            5
      • repeats      3
      • stratified   'list(disp = 3L, vs = NULL)'
      • orig data    mtcars2
      -- split info ------------------------------------------------------------------
      # A tibble: 15 x 3
         split             fold .repeat
         <list>           <int>   <int>
       1 <named list [2]>     1       1
       2 <named list [2]>     2       1
       3 <named list [2]>     3       1
       4 <named list [2]>     4       1
       5 <named list [2]>     5       1
       6 <named list [2]>     1       2
       7 <named list [2]>     2       2
       8 <named list [2]>     3       2
       9 <named list [2]>     4       2
      10 <named list [2]>     5       2
      11 <named list [2]>     1       3
      12 <named list [2]>     2       3
      13 <named list [2]>     3       3
      14 <named list [2]>     4       3
      15 <named list [2]>     5       3
      ================================================================================

