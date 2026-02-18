# Center and/or Scale Data

A function to center and/or scale a data matrix.

## Usage

``` r
center_scale(data, par_tbl = NULL, feat = NULL, center = TRUE, scale = TRUE)

is_center_scaled(data)

undo_center_scale(data, feat = NULL)
```

## Arguments

- data:

  A `tibble`, `data.frame`, or matrix object with named data variables
  to center and/or scale. If `matrix`, should *only* contain numeric
  data.

- par_tbl:

  A tibble containing the mean and standard deviations to use in
  processing the data. Must also contain an `feature` column to
  synchronize the features with their corresponding scaling parameters.
  If `NULL`, a parameter table is generated based on `data`, i.e. `data`
  is its own reference.

- feat:

  `character(n)`. A vector indicating which variables to center/scale.

- center:

  `logical(1)`. Whether the variables should be shifted to be zero
  centered (\\\mu = 0\\).

- scale:

  `logical(1)`. Whether the variables should be scaled to have unit
  variance (\\\sigma = 1\\).

## Value

A center/scaled object of the same class as `data`. Only features
specified in `feat` are modified.

## Functions

- `is_center_scaled()`: tests for presence of `par_tbl` entry in
  attributes and if it contains appropriate parameter information that
  can be used for centering or scaling data.

- `undo_center_scale()`: the inverse of `center_scale()`. Undo the
  transformation.

## Author

Stu Field

## Examples

``` r
scaled <- center_scale(mtcars)
apply(feature_matrix(scaled), 2, mean) |> sum()  # mean ~ 0
#> [1] -1.64365e-15
apply(feature_matrix(scaled), 2, sd)             # sd ~ 1
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>    1    1    1    1    1    1    1    1    1    1    1 

idx <- withr::with_seed(1,
  sample(1:nrow(mtcars), size = nrow(mtcars) / 2)
)
train <- mtcars[idx, ]
test  <- mtcars[-idx, ]

# Pass `par_tbl` as reference
ft <- c("disp", "hp", "drat")
par <- tibble::tibble(feature = ft,
                      means   = colMeans(feature_matrix(train, ft)),
                      sds     = apply(feature_matrix(train, ft), 2, sd))
cs <- center_scale(test, par_tbl = par)
# Logical test
is_center_scaled(cs)
#> [1] TRUE
# Example of `undo_center_scale()`; reverse above
old <- undo_center_scale(cs)

# check values are reverted
all.equal(test, old)
#> [1] "Attributes: < Component “class”: Lengths (1, 2) differ (string compare on first 1) >"
#> [2] "Attributes: < Component “class”: 1 string mismatch >"                                
```
