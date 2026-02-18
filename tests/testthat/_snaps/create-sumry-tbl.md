# the content is correct via snapshots

    Code
      tbl1
    Output
      # A tibble: 4 x 12
        cyl       n   NAs   min   max  mean    sd median   mad  mode   IQR     CV
        <chr> <int> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
      1 4        11     0  21.4  33.9  26.7  4.51   26    4.4   22.8  7.6  0.169 
      2 6         7     0  17.8  21.4  19.7  1.45   19.7  1.3   21    2.35 0.0736
      3 8        14     0  10.4  19.2  15.1  2.56   15.2  1.05  10.4  1.85 0.170 
      4 Total    32     0  10.4  33.9  20.1  6.03   19.2  3.65  10.4  7.38 0.300 

---

    Code
      tbl2
    Output
      # A tibble: 7 x 13
        cyl   am        n   NAs   min   max  mean    sd median   mad  mode   IQR
        <chr> <fct> <int> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
      1 4     0         3     0  21.5  24.4  22.9 1.45    22.8 1.3    21.5 1.45 
      2 4     1         8     0  21.4  33.9  28.1 4.48    28.8 3.2    30.4 5.7  
      3 6     0         4     0  17.8  21.4  19.1 1.63    18.6 0.700  17.8 1.72 
      4 6     1         3     0  19.7  21    20.6 0.751   21   0      21   0.650
      5 8     0        12     0  10.4  19.2  15.0 2.77    15.2 1.55   10.4 2.57 
      6 8     1         2     0  15    15.8  15.4 0.566   15.4 0.400  15   0.400
      7 Total <NA>     32     0  10.4  33.9  20.1 6.03    19.2 3.65   10.4 7.38 
      # i 1 more variable: CV <dbl>

