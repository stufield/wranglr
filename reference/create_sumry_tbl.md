# Create A Summary Table

Create a summary table of stratified by various grouping structures and
a pre-defined set of summary statistics:

- `min`

- `max`

- `mean`

- `sd` (standard deviation)

- `median`

- `mad` (median absolute deviation)

- `mode`

- `IQR`

- `CV`

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

  One or more unquoted column names containng grouping information.
  Passed to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

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
mt <- mutate(mtcars, cyl = as.factor(cyl), am = as.factor(am))

create_sumry_tbl(mt, mpg, cyl)
#> # A tibble: 4 × 12
#>   cyl       n   NAs   min   max  mean    sd median   mad  mode   IQR
#>   <chr> <int> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 4        11     0  21.4  33.9  26.7  4.51   26    4.4   22.8  7.6 
#> 2 6         7     0  17.8  21.4  19.7  1.45   19.7  1.3   21    2.35
#> 3 8        14     0  10.4  19.2  15.1  2.56   15.2  1.05  10.4  1.85
#> 4 Total    32     0  10.4  33.9  20.1  6.03   19.2  3.65  10.4  7.38
#> # ℹ 1 more variable: CV <dbl>

create_sumry_tbl(mt, mpg, cyl, am)
#> # A tibble: 7 × 13
#>   cyl   am        n   NAs   min   max  mean    sd median   mad  mode
#>   <chr> <fct> <int> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 4     0         3     0  21.5  24.4  22.9 1.45    22.8 1.3    21.5
#> 2 4     1         8     0  21.4  33.9  28.1 4.48    28.8 3.2    30.4
#> 3 6     0         4     0  17.8  21.4  19.1 1.63    18.6 0.700  17.8
#> 4 6     1         3     0  19.7  21    20.6 0.751   21   0      21  
#> 5 8     0        12     0  10.4  19.2  15.0 2.77    15.2 1.55   10.4
#> 6 8     1         2     0  15    15.8  15.4 0.566   15.4 0.400  15  
#> 7 Total NA       32     0  10.4  33.9  20.1 6.03    19.2 3.65   10.4
#> # ℹ 2 more variables: IQR <dbl>, CV <dbl>
```
