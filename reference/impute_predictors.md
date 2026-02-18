# Impute Predictor Variables

Threshold numeric values in model predictors according to set of
training ranges. Typically this involves capping values above or below a
threshold.

## Usage

``` r
impute_predictors(data, extrm_vals)
```

## Arguments

- data:

  A `data.frame`, or `tibble` object.

- extrm_vals:

  A `tibble` with the following **5** fields:

  `Feature:`

  :   feature name matching fields in `data`

  `xtrm_min:`

  :   minimum acceptable value for that feature

  `xtrm_max:`

  :   maximum acceptable value for that feature

  `impute_min:`

  :   value to assign if below `xtrm_min`

  `impute_max:`

  :   value to assign if above `xtrm_max`

  Use `NA` to *not* impute, or use
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  to remove the entire row if neither `min` nor `max` is desired for a
  given feature.

## See also

Other impute:
[`imputeNAs()`](https://stufield.github.io/wranglr/reference/imputeNAs.md),
[`impute_outliers()`](https://stufield.github.io/wranglr/reference/impute_outliers.md)

## Author

Stu Field

## Examples

``` r
x   <- data.frame(a = 1:3L, b = 4:6L, c = 7:9L, d = c(1.23, 4.56, 7.89))
tbl <- tibble::tribble(
  ~ Feature,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
    "a",         NA,        NA,           NA,         NA,
    "b",         5,         5,            0,          1,
    "c",         9,         7,            7.1,        7.1
)
impute_predictors(x, tbl)
#>   a b   c    d
#> 1 1 4 7.1 1.23
#> 2 2 5 8.0 4.56
#> 3 3 5 9.0 7.89
```
