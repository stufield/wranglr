# Remove Statistical Outliers

Remove statistical outliers from (optionally) *paired* vectors of
numeric values using a statistical criterion based on median absolute
deviation (\\6 \times mad\\) and a fold change criterion (5x the
median). See "outlier detection" section for more information about the
`type =` argument specification.

## Usage

``` r
remove_outliers(x, y = NULL, type = "nonparametric", ...)
```

## Arguments

- x:

  `numeric(n)`. A vector of numeric values.

- y:

  Optional. If `NULL`, assume non-paired data and performs outlier
  analysis on values in `x` along. If *not* `NULL`, either a numeric
  vector or character vector (e.g. class names) ordered in the same
  order as `x` indicating the pairing.

- type:

  `character(1)`. Matched. Either "parametric" or "nonparametric" to
  determine the type of outliers detection implementation.

- ...:

  Additional arguments passed to
  [`helpr::get_outliers()`](https://stufield.github.io/helpr/reference/get_outliers.html).

## Value

A `tibble` with columns `x` and `y` representing each numeric vector
pair with statistical outliers removed.

## outlier detection

There are 2 possible methods used to define an outlier measurement and
the return value depends on which method is implemented:

1.  The non-parametric case (default): agnostic to the distribution.
    Outlier measurements are defined as falling outside `mad_crit * mad`
    from the median *and* a specified number of fold-changes from the
    median (i.e. `fold_crit`; e.g. \\5x\\).  
    **Note:** `n_sigma` is ignored.

2.  The parametric case: the mean and standard deviation are calculated
    robustly via
    [`fit_gauss()`](https://stufield.github.io/helpr/reference/fit_gauss.html).
    Outliers are defined as measurements falling *outside* +/- `n_sigma`
    \* \\\sigma\\ from the the estimated \\\mu\\.  
    **Note:** `mad_crit` and `fold_crit` are ignored.

## See also

[`helpr::get_outliers()`](https://stufield.github.io/helpr/reference/get_outliers.html)

## Author

Stu Field

## Examples

``` r
x <- withr::with_seed(101, rnorm(10, mean = 1000, sd = 2))
x <- c(x, 10000)          # create outlier (11L)
x1 <- remove_outliers(x)  # 'x' only; no 'y'
x1
#> # A tibble: 10 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  999.    NA
#>  2 1001.    NA
#>  3  999.    NA
#>  4 1000.    NA
#>  5 1001.    NA
#>  6 1002.    NA
#>  7 1001.    NA
#>  8 1000.    NA
#>  9 1002.    NA
#> 10 1000.    NA

y  <- head(LETTERS, length(x))   # paired 'x' and 'y'
x2 <- remove_outliers(x, y)       # final row removed
x2
#> # A tibble: 10 × 2
#>        x y    
#>    <dbl> <chr>
#>  1  999. A    
#>  2 1001. B    
#>  3  999. C    
#>  4 1000. D    
#>  5 1001. E    
#>  6 1002. F    
#>  7 1001. G    
#>  8 1000. H    
#>  9 1002. I    
#> 10 1000. J    
```
