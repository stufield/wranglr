# Pre-processing data via \`create_recipe\`

## Introduction

Standard data transformations are common in preparing data for analysis.
Among the most common are:

- log-transformation
- centering with mean zero
- scaling to unit variance (`sd = 1`)

which are the typical data preparation steps for proteomic data.

Numerous tools exist in the `R` ecosystem, including some relatively new
packages, e.g. the [recipes](https://recipes.tidymodels.org) package,
however comes with significant implementation costs:

1.  `recipes` is in its infancy, still in a zero-point release
    (currently v1.3.1). As such, development is expected to be ongoing,
    resulting in likely interface changes and other underlying changes
    that would impact end-users. This represents a significant challenge
    to create a stable, production environment.
2.  `recipes` is designed with a broader use case in mind than proteomic
    data. As such, numerous trade-offs and generalizations must be made
    to accommodate various data sets it would be required to act upon.
3.  Many of its assumptions about data format, e.g. feature data vs meta
    data, and whether certain features are present in both the training
    set and test set are in direct conflict with existing workflows.
4.  `recipes` stores large portions of objects inside the object. While
    this may be appropriate for smaller, low-dimensional data, proteomic
    data with ~5000 features (stretch-goal of \> 10k) quickly becomes
    unmanageable and results in extremely large objects that must be
    saved and incorporated into our production environments for
    reproducibility.

Given this considerable technical overhead in adopting `recipes` as our
primary data transformation tool, and the relatively simple
transformations that proteomic data requires, the technical costs of
incorporating `recipes` into production level workflows do not seem to
balance the solutions it overcomes.

## Base example

We start with a typical pre-processing that can be achieved via
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
and `wranglr` S3 dplyr-methods tools. Combined with the function
[`log10()`](https://rdrr.io/r/base/Log.html), pre-processing can be as
simple as the following:

``` r
features <- c("mpg", "disp", "drat", "wt")
cs <- function(.x) {     # .x = numeric vector
  out <- log10(.x)       # log10
  out <- out - mean(out) # center
  out / sd(out)          # scale
}

# log10/center/scale features
mt_mod <- mtcars
for ( i in features ) {
  mt_mod[[i]] <- cs(mt_mod[[i]])
}

# check means of feature data
apply(feature_matrix(mt_mod, features), 2, mean) |> sum()  # mean = 0
#> [1] -9.072604e-16

# check sd of feature data
apply(feature_matrix(mt_mod, features), 2, sd)
#>  mpg disp drat   wt 
#>    1    1    1    1
```

### Preprocessing via `recipes`

Of course the same result can be achieved via the `recipes` package,
however not without some unintended consequences and type conversions:

``` r
rcp <- recipe(mtcars) |>
  step_log(all_of(features), base = 10) |>
  step_center(all_of(features)) |>
  step_scale(all_of(features)) |>
  prep()

rcp_data <- bake(rcp, mtcars)    # bake its own recipe

waldo::compare(mt_mod, rcp_data)
#> `class(old)`: "data.frame"                   
#> `class(new)`: "tbl_df"     "tbl" "data.frame"
#> 
#> `attr(old, 'row.names')` is a character vector ('Mazda RX4', 'Mazda RX4 Wag', 'Datsun 710', 'Hornet 4 Drive', 'Hornet Sportabout', ...)
#> `attr(new, 'row.names')` is an integer vector (1, 2, 3, 4, 5, ...)
#> 
#>      old$mpg             | new$mpg                 
#>  [1] 0.2921956512698269  - 0.2921956512698287  [1] 
#>  [2] 0.2921956512698269  - 0.2921956512698287  [2] 
#>  [3] 0.5683725639695327  - 0.5683725639695344  [3] 
#>  [4] 0.3555609266619139  - 0.3555609266619156  [4] 
#>  [5] -0.0973588568470361 - -0.0973588568470344 [5] 
#>  [6] -0.2068770390427560 - -0.2068770390427543 [6] 
#>  [7] -0.9982590624031733 - -0.9982590624031716 [7] 
#>  [8] 0.7961384738054255  - 0.7961384738054272  [8] 
#>  [9] 0.5683725639695327  - 0.5683725639695344  [9] 
#> [10] -0.0087452734091380 - -0.0087452734091362 [10]
#> [11] -0.2630052530305372 - -0.2630052530305355 [11]
#> [12] -0.5381042456559998 - -0.5381042456559980 [12]
#> [13] -0.3586886474273009 - -0.3586886474272992 [13]
#> [14] -0.7932846747726703 - -0.7932846747726685 [14]
#> [15] -2.0677094775208662 - -2.0677094775208644 [15]
#> [16] -2.0677094775208662 - -2.0677094775208644 [16]
#> [17] -0.9056115325793566 - -0.9056115325793548 [17]
#> [18] 1.7484580182372311  - 1.7484580182372329  [18]
#> [19] 1.5344837498075696  - 1.5344837498075714  [19]
#> [20] 1.9004414178059192  - 1.9004414178059210  [20]
#> [21] 0.3712171786197173  - 0.3712171786197190  [21]
#> [22] -0.7276488402979752 - -0.7276488402979735 [22]
#> [23] -0.7932846747726703 - -0.7932846747726685 [23]
#> [24] -1.2417178043577795 - -1.2417178043577779 [24]
#> [25] -0.0087452734091380 - -0.0087452734091362 [25]
#> [26] 1.1732830562164709  - 1.1732830562164727  [26]
#> [27] 1.0094330013234498  - 1.0094330013234514  [27]
#> [28] 1.5344837498075696  - 1.5344837498075714  [28]
#> [29] -0.6632712785493204 - -0.6632712785493188 [29]
#> [30] 0.0775900716318990  - 0.0775900716319007  [30]
#> [31] -0.8377655894612311 - -0.8377655894612295 [31]
#> [32] 0.3555609266619139  - 0.3555609266619156  [32]
#> 
#>      old$disp            | new$disp                
#>  [1] -0.3564005693460643 - -0.3564005693460608 [1] 
#>  [2] -0.3564005693460643 - -0.3564005693460608 [2] 
#>  [3] -1.0245268374438461 - -1.0245268374438423 [3] 
#>  [4] 0.4557791610670426  - 0.4557791610670459  [4] 
#>  [5] 1.0220856187645546  - 1.0220856187645579  [5] 
#>  [6] 0.2231346137075751  - 0.2231346137075785  [6] 
#>  [7] 1.0220856187645546  - 1.0220856187645579  [7] 
#>  [8] -0.5039234045752106 - -0.5039234045752069 [8] 
#>  [9] -0.5737022970189927 - -0.5737022970189890 [9] 
#> [10] -0.2775151823110336 - -0.2775151823110301 [10]
#> [11] -0.2775151823110336 - -0.2775151823110301 [11]
#> [12] 0.5691891772618392  - 0.5691891772618425  [12]
#> [13] 0.5691891772618392  - 0.5691891772618425  [13]
#> [14] 0.5691891772618392  - 0.5691891772618425  [14]
#> [15] 1.4825412480084112  - 1.4825412480084144  [15]
#> [16] 1.4387650272231167  - 1.4387650272231201  [16]
#> [17] 1.3632022481877379  - 1.3632022481877413  [17]
#> [18] -1.5625194129048876 - -1.5625194129048838 [18]
#> [19] -1.6285853427586641 - -1.6285853427586603 [19]
#> [20] -1.7351523848701897 - -1.7351523848701857 [20]
#> [21] -0.8440103481158979 - -0.8440103481158943 [21]
#> [22] 0.8112106761733330  - 0.8112106761733363  [22]
#> [23] 0.7346757022991656  - 0.7346757022991689  [23]
#> [24] 0.9741984343570680  - 0.9741984343570713  [24]
#> [25] 1.2011861291440837  - 1.2011861291440871  [25]
#> [26] -1.5560518744906613 - -1.5560518744906575 [26]
#> [27] -0.8411819237873273 - -0.8411819237873236 [27]
#> [28] -1.2407557454981515 - -1.2407557454981477 [28]
#> [29] 0.9790483167604191  - 0.9790483167604224  [29]
#> [30] -0.5237371391471235 - -0.5237371391471199 [30]
#> [31] 0.7178172239982828  - 0.7178172239982862  [31]
#> [32] -0.8313193363157336 - -0.8313193363157300 [32]
#> 
#>      old$drat            | new$drat                
#>  [1] 0.6174466768968913  - 0.6174466768968895  [1] 
#>  [2] 0.6174466768968913  - 0.6174466768968895  [2] 
#>  [3] 0.5305327519714770  - 0.5305327519714752  [3] 
#>  [4] -0.9725032383853297 - -0.9725032383853315 [4] 
#>  [5] -0.8211320203832367 - -0.8211320203832385 [5] 
#>  [6] -1.7114061543266874 - -1.7114061543266892 [6] 
#>  [7] -0.6940388965804515 - -0.6940388965804531 [7] 
#>  [8] 0.2446230640714255  - 0.2446230640714238  [8] 
#>  [9] 0.6519006431942563  - 0.6519006431942546  [9] 
#> [10] 0.6519006431942563  - 0.6519006431942546  [10]
#> [11] 0.6519006431942563  - 0.6519006431942546  [11]
#> [12] -0.9944080893158254 - -0.9944080893158271 [12]
#> [13] -0.9944080893158254 - -0.9944080893158271 [13]
#> [14] -0.9944080893158254 - -0.9944080893158271 [14]
#> [15] -1.3087994876449272 - -1.3087994876449289 [15]
#> [16] -1.1497696722005291 - -1.1497696722005306 [16]
#> [17] -0.6522018942222669 - -0.6522018942222686 [17]
#> [18] 0.9213660047362885  - 0.9213660047362867  [18]
#> [19] 2.1960501023433925  - 2.1960501023433907  [19]
#> [20] 1.1486171971444872  - 1.1486171971444854  [20]
#> [21] 0.2628524005697027  - 0.2628524005697009  [21]
#> [22] -1.7114061543266874 - -1.7114061543266892 [22]
#> [23] -0.8211320203832367 - -0.8211320203832385 [23]
#> [24] 0.3172462551798011  - 0.3172462551797994  [24]
#> [25] -0.9725032383853297 - -0.9725032383853315 [25]
#> [26] 0.9213660047362885  - 0.9213660047362867  [26]
#> [27] 1.4757345384430012  - 1.4757345384429994  [27]
#> [28] 0.3890947841471895  - 0.3890947841471877  [28]
#> [29] 1.1486171971444872  - 1.1486171971444854  [29]
#> [30] 0.1156173421847436  - 0.1156173421847419  [30]
#> [31] -0.0349081559688093 - -0.0349081559688110 [31]
#> [32] 0.9707122747061420  - 0.9707122747061403  [32]
#> 
#>      old$wt               | new$wt                   
#>  [1] -0.49941878390128880 - -0.49941878390128924 [1] 
#>  [2] -0.20688705243605401 - -0.20688705243605440 [2] 
#>  [3] -0.88243530421712701 - -0.88243530421712735 [3] 
#>  [4] 0.14516096952804006  - 0.14516096952803964  [4] 
#>  [5] 0.35821437027258324  - 0.35821437027258285  [5] 
#>  [6] 0.37647310070281231  - 0.37647310070281187  [6] 
#>  [7] 0.47504684719644458  - 0.47504684719644413  [7] 
#>  [8] 0.12057363015642972  - 0.12057363015642932  [8] 
#>  [9] 0.08083023621123864  - 0.08083023621123823  [9] 
#> [10] 0.35821437027258324  - 0.35821437027258285  [10]
#> [11] 0.35821437027258324  - 0.35821437027258285  [11]
#> [12] 0.88789114488029719  - 0.88789114488029686  [12]
#> [13] 0.61313459177967622  - 0.61313459177967589  [13]
#> [14] 0.65507425511759743  - 0.65507425511759698  [14]
#> [15] 1.68973795019723827  - 1.68973795019723783  [15]
#> [16] 1.79243287435869503  - 1.79243287435869458  [16]
#> [17] 1.74622159834988744  - 1.74622159834988699  [17]
#> [18] -1.04971118792495810 - -1.04971118792495854 [18]
#> [19] -2.02332996647317076 | -2.02332996647317076 [19]
#> [20] -1.62109321465024414 - -1.62109321465024458 [20]
#> [21] -0.69149065077775929 - -0.69149065077775973 [21]
#> [22] 0.43062266674731864  - 0.43062266674731819  [22]
#> [23] 0.35363310084151861  - 0.35363310084151817  [23]
#> [24] 0.70467562950020202  - 0.70467562950020157  [24]
#> [25] 0.70877403239280801  - 0.70877403239280767  [25]
#> [26] -1.45396530125925372 - -1.45396530125925416 [26]
#> [27] -1.13680306427445688 - -1.13680306427445732 [27]
#> [28] -2.22881296109019011 - -2.22881296109019056 [28]
#> [29] 0.10076462034095805  - 0.10076462034095765  [29]
#> [30] -0.32406986071302141 - -0.32406986071302185 [30]
#> [31] 0.47504684719644458  - 0.47504684719644413  [31]
#> [32] -0.31271985859783663 - -0.31271985859783707 [32]
```

The two objects are *NOT* the same … significant modifications have
occurred and perhaps most significantly, it is done so invisibly:

## An alternative workflow

The vast majority of proteomics pre-processing involves 3 main steps:

1.  log10-transformation
2.  center to $\mu = 0$
3.  scale to $\sigma = 1$

where this pre-processing is first applied to a training set and
therefore must also be applied to the test set. The new recipe ecosystem
defaults are set up to accommodate this workflow.

The goal is to provide users with the most beneficial aspects of
`recipes` without the imposed costs of stepping into the `recipes`
ecosystem. We will introduce 2 new user-facing functions:

- [`create_recipe()`](https://stufield.github.io/wranglr/dev/reference/create_recipe.md)
- [`bake_recipe()`](https://stufield.github.io/wranglr/dev/reference/create_recipe.md)

First, create train/test sets:

``` r
n      <- 5L
train  <- head(mtcars, -n)
test   <- tail(mtcars, n)
```

Set up the `rcp` processing controller object (as in `recipes`), however
it combines the
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
and
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
into one step:

``` r
# use the defaults: log10 -> center -> scale
# add some modifications on-the-fly: sqrt(), log10(), function(x)
rcp_ <- create_recipe(train,
                      feat = c("mpg", "disp", "drat", "wt"),
                      disp = sqrt,
                      hp   = log,
                      qsec = function(x) round(x / 10, 1L))

# new class object
class(rcp_)
#> [1] "rcp"  "list"

# S3 print
rcp_
#> ══ Pre-processing recipe ══════════════════════════════════════════════
#> 
#> ─── Training data:
#> ✓ Data containing 27 samples used in recipe
#> ✓ RFU features ( n = 4 ) will be processed by:
#> 
#> ─── Steps:
#> • log10-transformed      ✓
#> • Centered (mean = 0)    ✓
#> • Scaled (sd = 1)        ✓
#> ✓ Additional processing:
#> • disp                  sqrt
#> • hp                    log
#> • qsec                  function(x) round(x/10, 1L)
#> ═══════════════════════════════════════════════════════════════════════

# the recipe contains a processing parameter table
rcp_$par_tbl
#> # A tibble: 4 × 3
#>   feature means    sds
#>   <chr>   <dbl>  <dbl>
#> 1 mpg     1.28  0.133 
#> 2 disp    1.15  0.130 
#> 3 drat    0.545 0.0673
#> 4 wt      0.499 0.136
```

Compared to a `recipe` class object, a `rcp` is much smaller and
accomplishes the same task:

``` r
lobstr::obj_size(rcp)
#> 25.26 kB

lobstr::obj_size(rcp_)
#> 5.44 kB
```

Now apply the recipe in the familiar “bake” style:

``` r
baked_test <- bake_recipe(rcp_, test)

# convenient to know if data has been "baked"
is.baked(baked_test)
#> [1] TRUE

baked_test
#>                        mpg cyl       disp       hp       drat
#> Lotus Europa    1.51262140   4 -1.2504330 4.727388 0.46466494
#> Ford Pantera L -0.62969458   8  0.9282225 5.575949 1.19235920
#> Ferrari Dino    0.09247831   6 -0.5467058 5.164786 0.20264770
#> Maserati Bora  -0.79978722   8  0.6718339 5.814131 0.05843004
#> Volvo 142E      0.36343727   4 -0.8485863 4.691348 1.02190947
#>                         wt qsec vs am gear carb
#> Lotus Europa   -2.34080684  1.7  1  1    5    2
#> Ford Pantera L  0.01614505  1.4  0  1    5    4
#> Ferrari Dino   -0.41368156  1.6  0  1    5    6
#> Maserati Bora   0.39482538  1.5  0  1    5    8
#> Volvo 142E     -0.40219819  1.9  1  1    4    2
```

### What about non-Feature variables?

In the above example 3 variables have been processed via the `...`
mechanism. Arguments should be passed `...` in the form
`variable = function()` You may also pass anonymous functions
“on-the-fly”, where functions should take a vector input and return a
vector of the same length. It is important to realize that these
transformations take place *before* the center-scale transformation in
the case where features *and* “dot”-mechanism variables are processed.

- `disp`: was square rooted (prior to log \> center \> scale)
- `hp`: log_e-transformed
- `qseq`: anonymous function divide by 10 and round to 1 decimal place

``` r
# the 'additional' modified variables
rcp_$dot_vars
#> [1] "disp" "hp"   "qsec"

# compare modified meta data
head(test[, rcp_$dot_vars]) |> as_tibble()
#> # A tibble: 5 × 3
#>    disp    hp  qsec
#>   <dbl> <dbl> <dbl>
#> 1  95.1   113  16.9
#> 2 351     264  14.5
#> 3 145     175  15.5
#> 4 301     335  14.6
#> 5 121     109  18.6

head(baked_test[, rcp_$dot_vars]) |> as_tibble()
#> # A tibble: 5 × 3
#>     disp    hp  qsec
#>    <dbl> <dbl> <dbl>
#> 1 -1.25   4.73   1.7
#> 2  0.928  5.58   1.4
#> 3 -0.547  5.16   1.6
#> 4  0.672  5.81   1.5
#> 5 -0.849  4.69   1.9
```

------------------------------------------------------------------------

## Underlying machinery

The main underlying non-user-facing function to center and/or scale data
is
[`center_scale()`](https://stufield.github.io/wranglr/dev/reference/center_scale.md).

### `center_scale()`

``` r
cs_data <- log10(train) |> center_scale()

# check new class
is_center_scaled(cs_data)
#> [1] TRUE

# get the parameter table
pars <- attr(cs_data, "par_tbl")

# center/scale test based on train parameters
test_mod <- log10(test) |> center_scale(pars)
```

### Revert with `undo_center_scale()`

You can simply undo any center/scaling that has occurred via
[`undo_center_scale()`](https://stufield.github.io/wranglr/dev/reference/center_scale.md).

``` r
old <- undo_center_scale(test_mod)

# Safe-guards are in place so that you cannot double-undo:
#   `par_tbl` entry is removed from attributes
undo_center_scale(old)
#> Error in `undo_center_scale()`:
#> ! Must perform `undo` on previously center/scaled data.
```
