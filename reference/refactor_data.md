# Refactor Ghost Level Meta Data

Refactor the meta data fields in a data frame, removing the "ghost"
levels that remain after subsetting a factored column in a data frame.

## Usage

``` r
refactor_data(data)
```

## Arguments

- data:

  A `data.frame` object containing feature and clinical data.

## Value

A `data.frame` object, identical as the original except the factor
levels in factor class columns have been refactored to remove "ghost
levels".

## See also

[`factor()`](https://rdrr.io/r/base/factor.html),
[`droplevels()`](https://rdrr.io/r/base/droplevels.html)

## Author

Stu Field

## Examples

``` r
simdata$Sex <- factor(simdata$gender)
simdata$Sex
#>   [1] M F F M M F M M F F M M M F F M M M M F M M M F F F F M M F M F F
#>  [34] F M F F F M M F F M F F M M F F F M M M M M M F M M F F F F F M F
#>  [67] F M M F M M M F M M F F F M F F F F F M F M M F M F M M F M F F M
#> [100] F
#> Levels: F M

new <- simdata[simdata$Sex == "M", ]
new$Sex           # ghost levels!
#>  [1] M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M
#> [34] M M M M M M M M M M M M M M M M
#> Levels: F M
levels(new$Sex)   # ghost levels!
#> [1] "F" "M"

new2 <- refactor_data(new)
new2$Sex
#>  [1] M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M
#> [34] M M M M M M M M M M M M M M M M
#> Levels: M
levels(new2$Sex)
#> [1] "M"
```
