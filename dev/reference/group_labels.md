# Return Grouping Labels

Returns a `tibble` of the grouping labels (usually class labels)
corresponding a `grouped_df` class object.

## Usage

``` r
group_labels(x)
```

## Arguments

- x:

  A data frame, `tibble` or `grouped tibble`.

## See also

[`dplyr::group_vars()`](https://dplyr.tidyverse.org/reference/group_data.html),
[`dplyr::group_data()`](https://dplyr.tidyverse.org/reference/group_data.html)

## Author

Stu Field

## Examples

``` r
df <- dplyr::group_by(mtcars, cyl)
class(df)
#> [1] "grouped_df" "tbl_df"     "tbl"        "data.frame"
dplyr::group_data(df)
#> # A tibble: 3 × 2
#>     cyl       .rows
#>   <dbl> <list<int>>
#> 1     4        [11]
#> 2     6         [7]
#> 3     8        [14]
dplyr::group_vars(df)
#> [1] "cyl"

# 1 variable
group_labels(df)
#> # A tibble: 3 × 1
#>     cyl
#>   <dbl>
#> 1     4
#> 2     6
#> 3     8

# 2 variables
df <- dplyr::group_by(mtcars, cyl, vs)
group_labels(df)
#> # A tibble: 5 × 2
#>     cyl    vs
#>   <dbl> <dbl>
#> 1     4     0
#> 2     4     1
#> 3     6     0
#> 4     6     1
#> 5     8     0
```
