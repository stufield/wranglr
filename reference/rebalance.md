# Sampling for Class Imbalances

Implements up- or down-sampling for analyses with binary class
imbalances.

## Usage

``` r
rebalance(data, var, method = c("down", "up"))
```

## Arguments

- data:

  A data frame or tibble. For example, an individual "analysis" fold
  (`rsample::analysis()`) from an `rsample::vfold_cv()` call.

- var:

  `character(1)`. A column name of `data` containing class labels for
  up-/down-sampling. Assumes a binary variable. `tidyselect` variable
  selection is also supported (see examples).

- method:

  `character(1)`. One of "down" or "up" (matched). Down-sampling
  *decreases* size of the major class, whereas up-sampling *increases*
  (i.e. sampling with replacement) the size per group of the minor
  class.

## Value

A sub- or re-sampled data frame that is either smaller or larger than
`data` with equal class balance.

## Author

Stu Field

## Examples

``` r
xtab <- helpr::cross_tab

# original class distribution
xtab(mtcars, vs)
#> vs
#>   0   1 Sum 
#>  18  14  32 

# down-sampling `vs`
si <- rebalance(mtcars, "vs")    # 'down'
xtab(si, vs)
#> vs
#>   0   1 Sum 
#>  14  14  28 

# up-sampling `vs` & passed variable
var <- "vs"
si <- rebalance(mtcars, var, "up")
xtab(si, vs)
#> vs
#>   0   1 Sum 
#>  18  18  36 

# also supports unquoted strings
si  <- rebalance(mtcars, vs)
xtab(si, vs)
#> vs
#>   0   1 Sum 
#>  14  14  28 
```
