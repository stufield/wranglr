# Create k-Fold Partitioning

Adaptation of the `rsample::vfold_cv()` function and its utilities.
Modified for simplicity and to accommodate two-level stratification. Up
to 2 levels of stratification can be specified through the `breaks`
parameter:

- No stratification: `breaks = NULL`

- One level stratification: `breaks` is a `list(1)`, where the name of
  the list element specifies the stratification variable and the value
  of the element specifies the stratification.

- Two level stratification: `breaks` is a `list(2)`, where the list
  names specify the stratification variables and the corresponding
  values specify the stratification.

## Usage

``` r
create_kfold(data, k = 10L, repeats = 1L, breaks = NULL, ...)

is.k_split(x)

analysis(object, i = NULL)

assessment(object, i = NULL)
```

## Arguments

- data:

  A `data.frame` containing the data to be subset.

- k:

  `integer(1)`. The number of partitions (folds) to create.

- repeats:

  `integer(1)`The number of times to repeat the *k*-fold partitioning.

- breaks:

  Stratification control. Either `NULL` or a *named* list. If `NULL`, no
  stratification is performed. See `Details`.

- ...:

  For eventual extensibility. Parameters to be passed to internal
  stratification machinery but currently limited to a `depth` argument.
  The number of stratification bins is based on
  `min(5, floor(n / depth))`, where `n = length(x)`.

- x:

  An `R` object to test.

- object:

  A `k_split` object.

- i:

  `integer(1)` or `NULL`. If an integer, the split corresponding to the
  analysis or assessment data to be retrieved, otherwise a list of *all*
  data splits.

## Value

A `k_split` object. Element `data` contains the original data. Element
`splits` is a tibble where each row of corresponds to an individual
split. The `split` column contains lists named either "analysis" and
"assessment" containing the indices of `data` to be used for each fold
and category. Columns `fold` and `.repeat` provide fold and repeat
indices for each corresponding split.

`is.k_split()`: Logical. `TRUE` if `x` inherits from class `k_split`.

`analysis()`: A list of data frames corresponding to the analysis
indices.

`assessment()`: A list of data frames corresponding to the assessment
indices.

## Details

For stratification variables that are factor, character, or discrete
with 5 or fewer unique levels, the stratification structure should be
set to `NULL`. For example, if `status` is a binary variable,
`breaks = list(status = NULL)`.

If the stratification variable has more than 5 unique levels, the
stratification structure can be specified as either the number of
quantile-based stratification bins or as a numeric vector providing the
bin boundaries (must fully span the range of the stratification
variable). For example, for a continuous variable in `[0,1]`,
`breaks = list(x = 4)` indicates stratification into 4 bins, the
boundaries of which are determined internally using

    quantile(x, probs = seq(0.0, 1.0, length.out = 5))

and `breaks = list(x = c(0.0, 0.25, 0.75, 1.0))` specifies a 3 bin
structure: `[0, 0.25]`, `(0.25, 0.75]`, and `(0.75, 1.0]`. **Note:** the
lowest bin is *always* inclusive.

## Examples

``` r
# no stratification
no_strat <- create_kfold(mtcars, k = 4L, repeats = 2L)

# stratification on 1 discrete variable
sample_one <- create_kfold(mtcars, k = 4L, repeats = 2L,
                           breaks = list(vs = NULL))

# stratification on 2 variables; 1 continuous + 1 discrete
sample_two <- create_kfold(mtcars, k = 4L, repeats = 2L,
                           breaks = list(gear = 4L, vs = NULL))

# retrieve analysis data for 2nd split
an_2 <- analysis(no_strat, 2L)

# retrieve all splits
an_all <- analysis(no_strat)

# retrieve assessment data for 2nd split
ass_2 <- assessment(no_strat, 2L)

# retrieve all splits
ass_all <- assessment(no_strat)
```
