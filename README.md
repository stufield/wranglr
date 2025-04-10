
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `wranglr` package

<!-- badges: start -->

![GitHub
version](https://img.shields.io/badge/Version-0.0.1-success.svg?style=flat&logo=github)
[![CRAN
status](http://www.r-pkg.org/badges/version/wranglr)](https://cran.r-project.org/package=wranglr)
[![R-CMD-check](https://github.com/stufield/wranglr/workflows/R-CMD-check/badge.svg)](https://github.com/stufield/wranglr/actions)
[![](https://cranlogs.r-pkg.org/badges/grand-total/wranglr)](https://cran.r-project.org/package=wranglr)
[![Codecov test
coverage](https://codecov.io/gh/stufield/wranglr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stufield/wranglr?branch=main)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

## Overview

The `wranglr` package contains general functions necessary to manipulate
and wrangle internal `R` representations of proteomic data into
convenient forms for analysis.

------------------------------------------------------------------------

## Installation

``` r
# current dev version
remotes::install_github("stufield/wranglr")

# or a specific version
remotes::install_github("stufield/wranglr@v0.0.1")
```

------------------------------------------------------------------------

## Usage

To load `wranglr` simply make a call to `library()` as usual:

``` r
library(wranglr)
```

## Help summary of the package

``` r
library(help = wranglr)
```

------------------------------------------------------------------------

## Useful functions in `wranglr`

### Transforming Data

- `center_scale()`
- `create_recipe()`

``` r
scaled <- center_scale(mtcars)
apply(feature_matrix(scaled), 2, mean) |> sum()  # mean = 0
#> [1] 3.410697e-16
apply(feature_matrix(scaled), 2, sd)             # sd = 1
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>    1    1    1    1    1    1    1    1    1    1    1

# `create_recipe()`
rcp <- create_recipe(mtcars)
rcp
#> ══ Pre-processing recipe ═══════════════════════════════════════════════════════════════════════════
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
#> ════════════════════════════════════════════════════════════════════════════════════════════════════
```

------------------------------------------------------------------------

### Imputing Data

- `impute_outliers()`, `get_outliers()` (`helpr`)
- `imputeNAs()`
- `impute_predictors()`

``` r
# Outliers
x <- withr::with_seed(1, rnorm(10))   # normal
x <- c(x, 100)                        # add outlier
get_outliers(x)                       # index of the outlier
#> [1] 11

# parameters stored in attributes (parametric only)
attributes(get_outliers(x, type = "para"))
#> $mu
#> [1] 0.1157106
#> 
#> $sigma
#> [1] 0.9440787
#> 
#> $crit
#> [1] -2.716526  2.947947

# Impute 11th value
impute_outliers(x)
#>  [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078 -0.8204684  0.4874291  0.7383247
#>  [9]  0.5757814 -0.3053884  2.9479467

# Impute NAs
x <- withr::with_seed(1, rnorm(6))
x[ c(3, 5) ] <- NA
median(x, na.rm = TRUE)
#> [1] -0.2214052

imputeNAs(x)
#> [1] -0.6264538  0.1836433 -0.2214052  1.5952808 -0.2214052 -0.8204684

table(imputeNAs(x))
#> 
#> -0.820468384118015 -0.626453810742332 -0.221405243260125  0.183643324222082   1.59528080213779 
#>                  1                  1                  2                  1                  1

# Predictors
x   <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = c(1.23, 4.56, 7.89))
tbl <- tibble::tribble(
  ~ Feature,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
    "a",         NA,        NA,           NA,         NA,
    "b",         5,         5,            0,          1,
    "c",         9,         7,            7.1,        7.1
)
impute_predictors(x, tbl)
#>   a b   c    d
#> 1 1 4 7.1 1.23
#> 2 2 5 8.0 4.56
#> 3 3 5 9.0 7.89
```

------------------------------------------------------------------------

### Binding Data

- `bind_intersect()`
- `bind_union()`

``` r
df1 <- data.frame(a = 1, b = 2, c = 3, row.names = "A")
df2 <- data.frame(a = 4, b = 5, d = 6, row.names = "B")
df3 <- data.frame(a = 7, b = 8, e = 9, row.names = "C")
list_df  <- list(a = df1, b = df2, c = df3)
list_df
#> $a
#>   a b c
#> A 1 2 3
#> 
#> $b
#>   a b d
#> B 4 5 6
#> 
#> $c
#>   a b e
#> C 7 8 9

bind_intersect(list_df)
#>   data a b
#> A    a 1 2
#> B    b 4 5
#> C    c 7 8

bind_union(list_df)
#>   data a b  c  d  e
#> A    a 1 2  3 NA NA
#> B    b 4 5 NA  6 NA
#> C    c 7 8 NA NA  9
```

------------------------------------------------------------------------

### Selecting Feature Data

- `refactor_data()`
- `feature_matrix()`

``` r
df  <- data.frame(a = factor(c("a", "b")), b = 1:2L)
foo <- df[df$a == "a", ]
foo
#>   a b
#> 1 a 1

levels(foo$a)   # 2 levels! "b" is a ghost level
#> [1] "a" "b"

bar <- refactor_data(foo)
levels(bar$a)   # 1 level now
#> [1] "a"
```

------------------------------------------------------------------------

### Sequence IDs and Annotations

- `lookup_anno()`
- `seq_lookup()`
- `seqify()`

``` r
seqs <- withr::with_seed(101, sample(names(sample_df), 10L))
seqs
#>  [1] "seq.4500.50" "seq.2654.19" "seq.4993.16" "seq.3074.6"  "seq.4721.54" "seq.3516.60"
#>  [7] "seq.3194.36" "seq.3197.70" "seq.3448.13" "seq.4719.58"

# NAs for those analytes dropped from menu
seq_lookup(seqs)
#> # A tibble: 10 × 9
#>    seq         SeqId   EntrezGeneSymbol Target          TargetFullName Dilution UniProt List  Reason
#>    <chr>       <chr>   <chr>            <chr>           <chr>          <chr>    <chr>   <chr> <chr> 
#>  1 seq.4500.50 4500-50 CLEC11A          SCGF-alpha      Stem cell gro… 0.5%     Q9Y240  ""    ""    
#>  2 seq.2654.19 2654-19 TNFRSF1A         TNF sR-I        Tumor necrosi… 20%      P19438  ""    ""    
#>  3 seq.4993.16 4993-16 GSTA3            GSTA3           Glutathione S… 20%      Q16772  ""    ""    
#>  4 seq.3074.6  3074-6  LBP              LBP             Lipopolysacch… 0.005%   P18428  ""    ""    
#>  5 seq.4721.54 4721-54 TFF3             TFF3            Trefoil facto… 0.5%     Q07654  ""    ""    
#>  6 seq.3516.60 3516-60 CXCL12           SDF-1           Stromal cell-… 20%      P48061  ""    ""    
#>  7 seq.3194.36 3194-36 GP6              GPVI            Platelet glyc… 0.5%     Q9HCN6  ""    ""    
#>  8 seq.3197.70 3197-70 IDE              IDE             Insulin-degra… 20%      P14735  ""    ""    
#>  9 seq.3448.13 3448-13 INSR             IR              Insulin recep… 20%      P06213  ""    ""    
#> 10 seq.4719.58 4719-58 PDIA3            Protein disulf… Protein disul… 0.5%     P30101  ""    ""

seqify(seqs)
#> ══ SeqId Lookup ════════════════════════════════════════════════════════════════════════════════════
#>   SeqId-Feature     GeneID       Target                                                   List     Reason   
#> ────────────────────────────────────────────────────────────────────────────────────────────────────
#> ▶ seq.4500.50    ❯  CLEC11A   ❯  Stem cell growth factor-alpha                         ❯        ❯        
#> ▶ seq.2654.19    ❯  TNFRSF1A  ❯  Tumor necrosis factor receptor superfamily member 1A  ❯        ❯        
#> ▶ seq.4993.16    ❯  GSTA3     ❯  Glutathione S-transferase A3                          ❯        ❯        
#> ▶ seq.3074.6     ❯  LBP       ❯  Lipopolysaccharide-binding protein                    ❯        ❯        
#> ▶ seq.4721.54    ❯  TFF3      ❯  Trefoil factor 3                                      ❯        ❯        
#> ▶ seq.3516.60    ❯  CXCL12    ❯  Stromal cell-derived factor 1                         ❯        ❯        
#> ▶ seq.3194.36    ❯  GP6       ❯  Platelet glycoprotein VI                              ❯        ❯        
#> ▶ seq.3197.70    ❯  IDE       ❯  Insulin-degrading enzyme                              ❯        ❯        
#> ▶ seq.3448.13    ❯  INSR      ❯  Insulin receptor                                      ❯        ❯        
#> ▶ seq.4719.58    ❯  PDIA3     ❯  Protein disulfide-isomerase A3                        ❯        ❯

# Pass `tbl` containing annotations
# to reconstitute those missing ones
anno <- attr(sample_df, "anno")
seq_lookup(seqs, tbl = anno)
#> # A tibble: 10 × 5
#>    seq         SeqId   EntrezGeneSymbol Target                         TargetFullName               
#>    <chr>       <chr>   <chr>            <chr>                          <chr>                        
#>  1 seq.4500.50 4500-50 CLEC11A          SCGF-alpha                     Stem Cell Growth Factor-alpha
#>  2 seq.2654.19 2654-19 TNFRSF1A         TNF sR-I                       Tumor necrosis factor recept…
#>  3 seq.4993.16 4993-16 GSTA3            GSTA3                          Glutathione S-transferase A3 
#>  4 seq.3074.6  3074-6  LBP              LBP                            Lipopolysaccharide-binding p…
#>  5 seq.4721.54 4721-54 TFF3             TFF3                           Trefoil factor 3             
#>  6 seq.3516.60 3516-60 CXCL12           SDF-1                          Stromal cell-derived factor 1
#>  7 seq.3194.36 3194-36 GP6              GPVI                           Platelet glycoprotein VI     
#>  8 seq.3197.70 3197-70 IDE              IDE                            Insulin-degrading enzyme     
#>  9 seq.3448.13 3448-13 INSR             IR                             Insulin receptor             
#> 10 seq.4719.58 4719-58 PDIA3            Protein disulfide isomerase A3 Protein disulfide-isomerase …
```
