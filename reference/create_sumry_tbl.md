# Create A Summary Table

Create a summary table of stratified by various grouping structures and
a pre-defined set of summary statistics:

- `min`

- `max`

- `mean` (arithmetic mean)

- `sd` (standard deviation)

- `median` (50th percentile)

- `mad` (median absolute deviation)

- `mode` (most common value)

- `Q25` (lower quartile)

- `Q75` (upper quartile)

- `CV` (coefficient of variance)

## Usage

``` r
create_sumry_tbl(data, var, ...)
```

## Arguments

- data:

  A `data.frame` or `tibble` object containing data for summary.

- var:

  `character(1)`. An unquoted string containing the column name to
  summarize.

- ...:

  One or more unquoted column names containing grouping information.
  Passed to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  The first grouping column is coerced to `character` in the output to
  accommodate the summary "Total" row.

## Value

A `tibble` object with rows (groups) and columns as the standard summary
statistics.

## See also

[`mad()`](https://rdrr.io/r/stats/mad.html),
[`median()`](https://rdrr.io/r/stats/median.html),
[`sd()`](https://rdrr.io/r/stats/sd.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`IQR()`](https://rdrr.io/r/stats/IQR.html)

## Author

Stu Field

## Examples

``` r
create_sumry_tbl(mtcars, mpg, cyl)
#> # A tibble: 4 × 14
#>   cyl   total_n   NAs     n   min   max  mean    sd median   mad  mode
#>   <chr>   <int> <int> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 4          11     0    11  21.4  33.9  26.7  4.51   26    4.4   22.8
#> 2 6           7     0     7  17.8  21.4  19.7  1.45   19.7  1.3   21  
#> 3 8          14     0    14  10.4  19.2  15.1  2.56   15.2  1.05  10.4
#> 4 Total      32     0    32  10.4  33.9  20.1  6.03   19.2  3.65  10.4
#> # ℹ 3 more variables: Q25 <dbl>, Q75 <dbl>, CV <dbl>

create_sumry_tbl(mtcars, mpg, cyl, am)
#> # A tibble: 7 × 15
#>   cyl      am total_n   NAs     n   min   max  mean    sd median   mad
#>   <chr> <dbl>   <int> <int> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1 4         0       3     0     3  21.5  24.4  22.9 1.45    22.8 1.3  
#> 2 4         1       8     0     8  21.4  33.9  28.1 4.48    28.8 3.2  
#> 3 6         0       4     0     4  17.8  21.4  19.1 1.63    18.6 0.700
#> 4 6         1       3     0     3  19.7  21    20.6 0.751   21   0    
#> 5 8         0      12     0    12  10.4  19.2  15.0 2.77    15.2 1.55 
#> 6 8         1       2     0     2  15    15.8  15.4 0.566   15.4 0.400
#> 7 Total    NA      32     0    32  10.4  33.9  20.1 6.03    19.2 3.65 
#> # ℹ 4 more variables: mode <dbl>, Q25 <dbl>, Q75 <dbl>, CV <dbl>
```
