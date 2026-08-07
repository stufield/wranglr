# Impute Missing Values

There are two imputation strategies:

Imputing with the median is typically performed for numeric vectors,
however methods also exist for:

- the `factor` method imputes to the most common factor level.

- the `character` method imputes to the most common string

Imputing missing values from an estimated distribution is performed by
fitting via Maximum Likelihood and estimating and parameters, followed
by random draws from the appropriate random generator. Currently only
supports Gamma, Gaussian, and Beta distributions.

## Usage

``` r
impute_median(x)

impute_missing(x, distr = c("norm", "gamma", "beta"), seed = 123)
```

## Arguments

- x:

  A `data.frame`, matrix, or numeric vector. Factors or character
  strings are also possible but see caveat above.

- distr:

  `character(1)`. The name of the desired distribution.

- seed:

  `integer(1)`. Set a seed for reproducibility.

## Value

An object of the same dimension and class as `x` with any `NA` (missing)
values replaced.

## See also

[`median()`](https://rdrr.io/r/stats/median.html)

Other impute:
[`impute_outliers()`](https://stufield.github.io/wranglr/dev/reference/impute_outliers.md),
[`impute_predictors()`](https://stufield.github.io/wranglr/dev/reference/impute_predictors.md)

## Author

Stu Field

## Examples

``` r
# data frame method
df <- select(mtcars, mpg, cyl, disp) |> head()
df[cbind(2:3, 2:3)] <- NA_real_
df
#>                    mpg cyl disp
#> Mazda RX4         21.0   6  160
#> Mazda RX4 Wag     21.0  NA  160
#> Datsun 710        22.8   4   NA
#> Hornet 4 Drive    21.4   6  258
#> Hornet Sportabout 18.7   8  360
#> Valiant           18.1   6  225

impute_median(df)
#>                    mpg cyl disp
#> Mazda RX4         21.0   6  160
#> Mazda RX4 Wag     21.0   6  160
#> Datsun 710        22.8   4  225
#> Hornet 4 Drive    21.4   6  258
#> Hornet Sportabout 18.7   8  360
#> Valiant           18.1   6  225
x <- rnorm(10, mean = 10, sd = 5)
x[c(2, 7)] <- NA_real_
y <- rbeta(10, shape1 = 2, shape2 = 3)
y[c(2, 7)] <- NA_real_

data.frame(
  orig_x    = x,
  imputed_x = impute_missing(x), # gaussian
  orig_y    = y,
  imputed_y = impute_missing(y, "beta") # beta
)
#>       orig_x imputed_x     orig_y  imputed_y
#> 1   7.105577  7.105577 0.33386926 0.33386926
#> 2         NA  9.971782         NA 0.18073603
#> 3  10.664961 10.664961 0.41652882 0.41652882
#> 4  11.882497 11.882497 0.16576899 0.16576899
#> 5  15.693538 15.693538 0.25369523 0.25369523
#> 6  16.206315 16.206315 0.04883205 0.04883205
#> 7         NA 11.156292         NA 0.23837690
#> 8   7.853100  7.853100 0.31795159 0.31795159
#> 9  16.802307 16.802307 0.60805309 0.60805309
#> 10  9.645713  9.645713 0.13826012 0.13826012
```
