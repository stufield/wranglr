# Convert Table Entries to Numeric

Selectively convert an R object to a numeric vector if doing so does
*not* result in a warning. This is typically the case when a `character`
string contains a symbol that cannot be coerced cleanly. The following
rules apply during conversion:

- characters converted if no warnings are triggered

- integers are *never* converted

- logical vectors *always* converted to `0` or `1`

- factors are *optionally* converted to numeric

## Usage

``` r
cast_numeric(x, ...)

# S3 method for class 'character'
cast_numeric(x, ...)

# S3 method for class 'factor'
cast_numeric(x, ..., coerce_factor = TRUE)

# S3 method for class 'integer'
cast_numeric(x, ...)

# S3 method for class 'logical'
cast_numeric(x, ...)

# S3 method for class 'list'
cast_numeric(x, ..., coerce_factor = TRUE)

# S3 method for class 'data.frame'
cast_numeric(x, ..., coerce_factor = TRUE)
```

## Arguments

- x:

  An object for S3 dispatch. Usually a character, data frame, tibble, or
  named list containing convertible columns.

- ...:

  Additional arguments to allow extensibility to S3 methods.

- coerce_factor:

  `logical(1)`. Should `factor` types be converted to their
  corresponding numeric?

## Value

An object coerced to a numeric if the rules above can be followed. A
data frame, `tibble`, or list returns the same class object with the
function applied to each element (column).

## See also

[`as.numeric()`](https://rdrr.io/r/base/numeric.html)

## Author

Stu Field

## Examples

``` r
tbl <- tibble::tibble(
  id     = 1:20,                            # NO
  chr_id = as.character(1:20),              # YES
  logic  = sample(c(TRUE, FALSE), 20, replace = TRUE), # YES
  fact   = factor(letters[1:20]),           # YES or NO depending on coerce.factor
  num    = rnorm(20),                       # YES
  x      = rep(c("foo", "bar"), each = 10), # NO
  y      = c(as.character(runif(19)), NA),  # YES; only 1 NA, 5% total
  z      = as.character(rnorm(20))          # YES
)
cast_numeric(tbl)
#> # A tibble: 20 × 8
#>       id chr_id logic  fact     num x           y         z
#>  * <int>  <dbl> <dbl> <dbl>   <dbl> <chr>   <dbl>     <dbl>
#>  1     1      1     1     1  0.512  foo    0.545  -0.795   
#>  2     2      2     0     2 -1.86   foo    0.279  -1.57    
#>  3     3      3     0     3 -0.522  foo    0.447  -1.04    
#>  4     4      4     1     4 -0.0526 foo    0.372   1.02    
#>  5     5      5     1     5  0.543  foo    0.0281 -0.702   
#>  6     6      6     1     6 -0.914  foo    0.466   0.973   
#>  7     7      7     1     7  0.468  foo    0.390  -0.0768  
#>  8     8      8     0     8  0.363  foo    0.0201  0.893   
#>  9     9      9     1     9 -1.30   foo    0.377  -0.778   
#> 10    10     10     1    10  0.738  foo    0.560   0.437   
#> 11    11     11     0    11  1.89   bar    0.857   0.413   
#> 12    12     12     0    12 -0.0974 bar    0.385   0.976   
#> 13    13     13     1    13 -0.936  bar    0.528   1.15    
#> 14    14     14     1    14 -0.0160 bar    0.601   1.22    
#> 15    15     15     0    15 -0.827  bar    0.261   0.000480
#> 16    16     16     1    16 -1.51   bar    0.290   0.755   
#> 17    17     17     1    17  0.935  bar    0.480   0.342   
#> 18    18     18     1    18  0.176  bar    0.920   0.168   
#> 19    19     19     0    19  0.244  bar    0.401   1.40    
#> 20    20     20     1    20  1.62   bar   NA      -0.679   
```
