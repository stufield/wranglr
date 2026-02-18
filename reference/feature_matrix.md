# Select the Feature Matrix

Selects the feature (variables) data from a `data.frame` object and
returns a data matrix of only the feature (usually numeric) data.
Particularly useful in selecting prior to
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) methods that
require a named, numeric data matrix as input.

## Usage

``` r
feature_matrix(data, feat = NULL)
```

## Arguments

- data:

  A `data.frame` object containing numeric feature data.

- feat:

  `charcter(n)`. A vector of column names corresponding to the features.

## Value

A numeric `data.matrix` object containing only the data matrix of values
for the features.

## See also

[`data.matrix()`](https://rdrr.io/r/base/data.matrix.html)

## Author

Stu Field

## Examples

``` r
dim(simdata)
#> [1] 100  55
class(simdata)
#> [1] "soma_adat"  "data.frame"

feature_matrix(simdata) |> dim()
#> [1] 100  40
feature_matrix(simdata) |> class()
#> [1] "matrix" "array" 
```
