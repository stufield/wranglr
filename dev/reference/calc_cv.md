# Calculate CV Decomposition

Calculate the appropriate within and between assay run variance
decomposition prior to calculating the %CV for a reproducibility study
assay run.

## Usage

``` r
calc_cv(x)
```

## Arguments

- x:

  A list of numeric vectors, each representing a separate assay run,
  i.e. a technical replicate. The entries of the vectors should be
  replicates of intra-run assay variation. **Important!**: be sure to
  remove any `NAs`, if present, beforehand.

## Value

A named vector of the CV decomposition for the replicates.

## See also

[`helpr::calc_ss()`](https://stufield.github.io/helpr/reference/calc_ss.html)

## Author

Stu Field

## Examples

``` r
# Example 1
example <- list(A = c(5785.1, 5005.6, 5686.3, 5990.8,
                      5235.4, 5340.6, 5272.6, 5905.2),
                B = c(5708.0, 5300.7, 5339.8, 5393.0,
                      5762.0, 5553.4, 6081.4, 5473.5),
                C = c(5409.4, 5353.6, 5398.0, 5631.2,
                      5646.1, 5073.4, 5879.2, 5617.5))
example
#> $A
#> [1] 5785.1 5005.6 5686.3 5990.8 5235.4 5340.6 5272.6 5905.2
#> 
#> $B
#> [1] 5708.0 5300.7 5339.8 5393.0 5762.0 5553.4 6081.4 5473.5
#> 
#> $C
#> [1] 5409.4 5353.6 5398.0 5631.2 5646.1 5073.4 5879.2 5617.5
#> 

calc_cv(example)
#>       Intra       Inter       Total 
#> 0.050656604 0.005763662 0.050983441 

# Example 2 (dummy `simdata` example)
# look at how the intra, inter, and total variance.
# We assume `simdata` contains *only* replicate samples
# and assume the `SiteId` represents assay "runs".
# We can then `lapply()` over each feature, calculating
# the `CVdecomp` for each and store to a data frame
split_on <- simdata$SiteId

# split data
site_split <- feature_matrix(simdata) |> data.frame() |>
  lapply(split, f = split_on)

# iterate over list/analytes; each contains multiple (4x) "runs"
coeffVar <- lapply(site_split, calc_cv)
coeffVar <- do.call(rbind, coeffVar) |> data.frame()
```
