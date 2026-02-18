# Pre-processing Analysis Data

Relative too the recipes package, the
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
and
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
functions are combined.

## Usage

``` r
create_recipe(
  data,
  feat = NULL,
  log10 = TRUE,
  center = TRUE,
  scale = TRUE,
  ...
)

bake_recipe(x, data)

is.baked(data)

convert_recipe(object)
```

## Arguments

- data:

  A `data.frame` to be pre-processed according to `x`.

- feat:

  `character(n)`. A vector indicating which variables to center/scale.

- log10:

  `logical(1)`. Should features in `data` be log10-transformed?

- center:

  `logical(1)`. Whether the variables should be shifted to be zero
  centered (\\\mu = 0\\).

- scale:

  `logical(1)`. Whether the variables should be scaled to have unit
  variance (\\\sigma = 1\\).

- ...:

  Optional arguments of the form `variable = function()` specifying the
  function to be applied to the specified column. Anonymous functions
  can be used but they should take a vector input and return a vector of
  equal length. See Examples. If also center/scaling, the `...`
  transformation takes place *before* the center-scale step.

- x:

  A `rcp` class object with instructions for pre-processing.

- object:

  A `recipe` class object from the `recipes` package with the 3
  pre-processing steps described above.

## Value

A `rcp` class object containing information for the recipe steps.

## Details

The order of recipe steps is *always*:

1.  log10-transform

2.  center

3.  scale

## Functions

- `bake_recipe()`: executes the recipe instructions defined during via
  `create_recipe()`.

- `is.baked()`: tests for presence of `baked` entry in attributes,
  indicating that the data have already been baked via `bake_recipe()`.

- `convert_recipe()`: converts an existing recipe object from the
  recipes package into a `rcp` object. Note that *all* conversions and
  not possible and the intended use case is for 3 step conversion:

  - log10-transform

  - centered

  - scaled

## Examples

``` r
# create a pre-processing recipe
rcp <- create_recipe(mtcars)
rcp
#> ══ Pre-processing recipe ══════════════════════════════════════════════
#> 
#> ─── Training data:
#> ✓ Data containing 32 samples used in recipe
#> ✓ RFU features ( n = 11 ) will be processed by:
#> 
#> ─── Steps:
#> • log10-transformed      ✓
#> • Centered (mean = 0)    ✓
#> • Scaled (sd = 1)        ✓
#> 
#> ═══════════════════════════════════════════════════════════════════════

rcp2 <- create_recipe(mtcars,
                      feat = c("mpg", "disp", "drat", "wt"),
                      disp = abs,
                      hp   = log10,
                      qsec = function(x) round(x / 10))

# apply recipe to orig/own data set
new_data <- bake_recipe(rcp, mtcars)

# Logical test
is.baked(new_data)
#> [1] TRUE

# converting recipes
library(recipes)
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
rec <- recipe(Species ~ ., data = iris) |>
  step_log(all_predictors(), base = 10) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  prep()

convert_recipe(rec)
#> ══ Pre-processing recipe ══════════════════════════════════════════════
#> 
#> ─── Training data:
#> ✓ Data containing 150 samples used in recipe
#> ✓ RFU features ( n = 4 ) will be processed by:
#> 
#> ─── Steps:
#> • log10-transformed      ✓
#> • Centered (mean = 0)    ✓
#> • Scaled (sd = 1)        ✓
#> 
#> ═══════════════════════════════════════════════════════════════════════
```
