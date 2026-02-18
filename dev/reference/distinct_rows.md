# Select Distinct Rows

Extract distinct rows by selection variables. Additional arguments allow
specification of variables that are combined and checked for uniqueness.

## Usage

``` r
distinct_rows(x, ...)
```

## Arguments

- x:

  A data frame or tibble object.

- ...:

  Additional arguments to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

## Value

A data frame or tibble object, same class as `x`.

## Author

Stu Field

## Examples

``` r
withr::with_seed(111, {
  df <- tibble::tibble(
    a_ie = sample(letters[1:3L], 20, replace = TRUE),
    b_ie = sample(1:3, 20, replace = TRUE),
    var  = runif(20)
  )
})
df
#> # A tibble: 20 × 3
#>    a_ie   b_ie      var
#>    <chr> <int>    <dbl>
#>  1 b         3 0.548   
#>  2 c         1 0.576   
#>  3 c         3 0.456   
#>  4 c         1 0.0966  
#>  5 a         1 0.806   
#>  6 c         3 0.000925
#>  7 a         1 0.467   
#>  8 c         3 0.173   
#>  9 b         3 0.259   
#> 10 a         2 0.919   
#> 11 a         1 0.232   
#> 12 a         1 0.0526  
#> 13 b         3 0.304   
#> 14 c         2 0.0117  
#> 15 b         2 0.301   
#> 16 b         1 0.878   
#> 17 a         1 0.665   
#> 18 c         1 0.454   
#> 19 a         2 0.0533  
#> 20 c         1 0.631   

distinct_rows(df, a_ie, b_ie)
#> # A tibble: 8 × 3
#>   a_ie   b_ie    var
#>   <chr> <int>  <dbl>
#> 1 b         3 0.548 
#> 2 c         1 0.576 
#> 3 c         3 0.456 
#> 4 a         1 0.806 
#> 5 a         2 0.919 
#> 6 c         2 0.0117
#> 7 b         2 0.301 
#> 8 b         1 0.878 

# or with tidyselect-helpers
distinct_rows(df, ends_with("ie"))
#> # A tibble: 8 × 3
#>   a_ie   b_ie    var
#>   <chr> <int>  <dbl>
#> 1 b         3 0.548 
#> 2 c         1 0.576 
#> 3 c         3 0.456 
#> 4 a         1 0.806 
#> 5 a         2 0.919 
#> 6 c         2 0.0117
#> 7 b         2 0.301 
#> 8 b         1 0.878 
```
