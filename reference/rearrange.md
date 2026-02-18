# Rearrange Rows by Variables

Rearrange (reorder) and possibly filter the rows of a data frame or
tibble object according to a matched vector. Similar to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
however the ordering is based on an external object `x`, rather than a
variable contained in `tbl`.

## Usage

``` r
rearrange(tbl, var, x)
```

## Arguments

- tbl:

  A data frame or tibble object.

- var:

  Character. The variable name (column).

- x:

  A vector of values used to sort and match.

## Value

A reordered, and possibly filtered, object of the same class as `tbl`.

## Details

This function is meant to act as a working solution to the lack of row
names inherent to tibble objects, yet still plays nice with `tidyverse`
syntax.

Values in `x` that are not in `tbl[[var]]` are silently dropped. Values
in `tbl[[var]]` that are not in `x` are silently filtered.

Duplicates in `x` are silently dropped to avoid creating extra rows in
`tbl`, whereas duplicates in `var` are maintained.

## See also

[`match()`](https://rdrr.io/r/base/match.html)

## Author

Stu Field

## Examples

``` r
df <- tibble::tibble(name = letters[1:4L], x = 1:4, y = rnorm(4))
df
#> # A tibble: 4 × 3
#>   name      x      y
#>   <chr> <int>  <dbl>
#> 1 a         1  0.559
#> 2 b         2  0.415
#> 3 c         3 -1.45 
#> 4 d         4  0.941

rearrange(df, "name", c("c", "b", "d"))
#> # A tibble: 3 × 3
#>   name      x      y
#>   <chr> <int>  <dbl>
#> 1 c         3 -1.45 
#> 2 b         2  0.415
#> 3 d         4  0.941

rearrange(df, "name", "d")
#> # A tibble: 1 × 3
#>   name      x     y
#>   <chr> <int> <dbl>
#> 1 d         4 0.941

rearrange(df, "name", c("a", "c", "z"))  # "z" is ignored
#> # A tibble: 2 × 3
#>   name      x      y
#>   <chr> <int>  <dbl>
#> 1 a         1  0.559
#> 2 c         3 -1.45 

rearrange(df, "name", c("a", "c", "c"))  # duplicate "c" is dropped
#> # A tibble: 2 × 3
#>   name      x      y
#>   <chr> <int>  <dbl>
#> 1 a         1  0.559
#> 2 c         3 -1.45 
```
