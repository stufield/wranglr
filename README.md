
<!-- README.md is generated from README.Rmd. Please edit that file -->

    ## [1] "en_US.UTF-8"

# The `splyr` package

<!-- badges: start -->

![GitHub
version](https://img.shields.io/badge/Version-0.0.1-success.svg?style=flat&logo=github)
[![CRAN
status](http://www.r-pkg.org/badges/version/splyr)](https://cran.r-project.org/package=splyr)
[![R-CMD-check](https://github.com/stufield/splyr/workflows/R-CMD-check/badge.svg)](https://github.com/stufield/splyr/actions)
[![](https://cranlogs.r-pkg.org/badges/grand-total/splyr)](https://cran.r-project.org/package=splyr)
[![Codecov test
coverage](https://codecov.io/gh/stufield/splyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stufield/splyr?branch=main)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

## Overview

The `splyr` package contains the general functions necessary to
manipulate internal `R` representations of `*.adat` files and wrangle
those data into convenient form for analysis.

------------------------------------------------------------------------

## Installation

``` r
remotes::install_github("splyr")
```

------------------------------------------------------------------------

## Usage

To load `splyr` simply make a call to `library()` as usual:

``` r
library(splyr)
```

## Help summary of the package

``` r
library(help = splyr)
```

------------------------------------------------------------------------

## Useful functions in `splyr`

### Transforming Data

- `center_scale()`
- `create_recipe()`

``` r
scaled <- center_scale(sim_adat)
apply(feature_matrix(scaled), 2, mean) |> sum()  # mean = 0
#> [1] -1.049005e-15
apply(feature_matrix(scaled), 2, sd)             # sd = 1
#> seq.2802.68 seq.9251.29 seq.1942.70 seq.5751.80 seq.9608.12 seq.3459.49 seq.3865.56 seq.3363.21 
#>           1           1           1           1           1           1           1           1 
#> seq.4487.88 seq.5994.84 seq.9011.72 seq.2902.23 seq.2260.48 seq.4936.96 seq.2277.95 seq.2953.31 
#>           1           1           1           1           1           1           1           1 
#> seq.3032.11  seq.4330.4 seq.4914.10  seq.3896.5  seq.5002.7  seq.3476.4 seq.1130.49 seq.6356.60 
#>           1           1           1           1           1           1           1           1 
#> seq.4579.40 seq.8344.24 seq.8441.53 seq.9360.55  seq.7841.8 seq.8142.63 seq.4461.56 seq.9297.97 
#>           1           1           1           1           1           1           1           1 
#> seq.9396.38 seq.3300.26 seq.2772.14 seq.6615.18 seq.8797.98 seq.9879.88 seq.8993.16 seq.9373.82 
#>           1           1           1           1           1           1           1           1

# `create_recipe()`
rcp <- create_recipe(sim_adat)
#> Warning in lapply(X = x, FUN = .Generic, ...): NaNs produced
rcp
#> ══ Pre-processing recipe ═══════════════════════════════════════════════════════════════════════════
#> 
#> ─── Training data:
#> ✓ Data containing 100 samples used in recipe✓ RFU features ( n = 49 ) will be processed by:
#> 
#> ─── Steps:
#> • log10-transformed ✓• Centered (mean = 0) ✓• Scaled (sd = 1) ✓
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

### `seq.XXXX` formats

- `seq_lookup()`
- `seqify()`
- `lookup_anno()`

``` r
seqs <- withr::with_seed(101, sample(names(sample_df), 10L))
seqs
#>  [1] "seq.4493.92" "seq.2644.11" "seq.4987.17" "seq.3066.12" "seq.4706.17" "seq.3504.58"
#>  [7] "seq.3186.2"  "seq.3190.43" "seq.3440.7"  "seq.4703.87"

# NAs for those analytes dropped from menu
seq_lookup(seqs)
#> # A tibble: 10 × 10
#>    seq         SeqId   EntrezGeneSymbol Target    TargetFullName Type  Dilution UniProt List  Reason
#>    <chr>       <chr>   <chr>            <chr>     <chr>          <chr> <chr>    <chr>   <chr> <chr> 
#>  1 seq.4493.92 4493-92 IL11             IL-11     Interleukin-11 Prot… 20%      P20809  ""    ""    
#>  2 seq.2644.11 2644-11 PRKCA            PKC-A     Protein kinas… Prot… 20%      P17252  ""    ""    
#>  3 seq.4987.17 4987-17 FCAR             FCAR      Immunoglobuli… Prot… 20%      P24071  ""    ""    
#>  4 seq.3066.12 3066-12 LGALS3           Galectin… Galectin-3     Prot… 0.5%     P17931  ""    ""    
#>  5 seq.4706.17 4706-17 EPB41            41        Protein 4.1    Prot… 0.5%     P11171  ""    ""    
#>  6 seq.3504.58 3504-58 HAMP             LEAP-1    Hepcidin       Prot… 0.5%     P81172  ""    ""    
#>  7 seq.3186.2  3186-2  C2               C2        Complement C2  Prot… 0.005%   P06681  ""    ""    
#>  8 seq.3190.43 3190-43 <NA>             <NA>      <NA>           <NA>  <NA>     <NA>    <NA>  <NA>  
#>  9 seq.3440.7  3440-7  GZMA             granzyme… Granzyme A     Prot… 20%      P12544  ""    ""    
#> 10 seq.4703.87 4703-87 LTA              TNF-b     Lymphotoxin-a… Prot… 20%      P01374  ""    ""

seqify(seqs)
#> ══ SeqId Lookup ════════════════════════════════════════════════════════════════════════════════════
#>   SeqId-Feature     GeneID     Target                               List     Reason   
#> ────────────────────────────────────────────────────────────────────────────────────────────────────
#> ▶ seq.4493.92    ❯  IL11    ❯  Interleukin-11                    ❯        ❯        
#> ▶ seq.2644.11    ❯  PRKCA   ❯  Protein kinase C alpha type       ❯        ❯        
#> ▶ seq.4987.17    ❯  FCAR    ❯  Immunoglobulin alpha Fc receptor  ❯        ❯        
#> ▶ seq.3066.12    ❯  LGALS3  ❯  Galectin-3                        ❯        ❯        
#> ▶ seq.4706.17    ❯  EPB41   ❯  Protein 4.1                       ❯        ❯        
#> ▶ seq.3504.58    ❯  HAMP    ❯  Hepcidin                          ❯        ❯        
#> ▶ seq.3186.2     ❯  C2      ❯  Complement C2                     ❯        ❯        
#> ▶ seq.3190.43    ❯  NA      ❯  NA                                ❯  NA    ❯  NA    
#> ▶ seq.3440.7     ❯  GZMA    ❯  Granzyme A                        ❯        ❯        
#> ▶ seq.4703.87    ❯  LTA     ❯  Lymphotoxin-alpha                 ❯        ❯

# Pass `tbl` containing annotations
# to reconstitute those missing ones
seq_lookup(seqs, splyr:::sample_cm)
#> # A tibble: 10 × 8
#>    seq         SeqId   EntrezGeneSymbol Target     TargetFullName             Type  Dilution UniProt
#>    <chr>       <chr>   <chr>            <chr>      <chr>                      <chr> <chr>    <chr>  
#>  1 seq.4493.92 4493-92 IL11             IL-11      Interleukin-11             Prot… 40       P20809 
#>  2 seq.2644.11 2644-11 PRKCA            PKC-A      Protein kinase C alpha ty… Prot… 40       P17252 
#>  3 seq.4987.17 4987-17 FCAR             FCAR       Immunoglobulin alpha Fc r… Prot… 40       P24071 
#>  4 seq.3066.12 3066-12 LGALS3           Galectin-3 Galectin-3                 Prot… 1        P17931 
#>  5 seq.4706.17 4706-17 EPB41            41         Protein 4.1                Prot… 1        P11171 
#>  6 seq.3504.58 3504-58 HAMP             LEAP-1     Hepcidin                   Prot… 1        P81172 
#>  7 seq.3186.2  3186-2  C2               C2         Complement C2              Prot… 0.005    P06681 
#>  8 seq.3190.43 3190-43 GAS1             GAS1       Growth arrest-specific pr… Prot… 40       P54826 
#>  9 seq.3440.7  3440-7  GZMA             granzyme A Granzyme A                 Prot… 40       P12544 
#> 10 seq.4703.87 4703-87 LTA              TNF-b      Lymphotoxin-alpha          Prot… 40       P01374

# OR: on the fly
seq_lookup(seqs, attr(sample_df, "Col.Meta"))
#> # A tibble: 10 × 8
#>    seq         SeqId   EntrezGeneSymbol Target     TargetFullName             Type  Dilution UniProt
#>    <chr>       <chr>   <chr>            <chr>      <chr>                      <chr> <chr>    <chr>  
#>  1 seq.4493.92 4493-92 IL11             IL-11      Interleukin-11             Prot… 40       P20809 
#>  2 seq.2644.11 2644-11 PRKCA            PKC-A      Protein kinase C alpha ty… Prot… 40       P17252 
#>  3 seq.4987.17 4987-17 FCAR             FCAR       Immunoglobulin alpha Fc r… Prot… 40       P24071 
#>  4 seq.3066.12 3066-12 LGALS3           Galectin-3 Galectin-3                 Prot… 1        P17931 
#>  5 seq.4706.17 4706-17 EPB41            41         Protein 4.1                Prot… 1        P11171 
#>  6 seq.3504.58 3504-58 HAMP             LEAP-1     Hepcidin                   Prot… 1        P81172 
#>  7 seq.3186.2  3186-2  C2               C2         Complement C2              Prot… 0.005    P06681 
#>  8 seq.3190.43 3190-43 GAS1             GAS1       Growth arrest-specific pr… Prot… 40       P54826 
#>  9 seq.3440.7  3440-7  GZMA             granzyme A Granzyme A                 Prot… 40       P12544 
#> 10 seq.4703.87 4703-87 LTA              TNF-b      Lymphotoxin-alpha          Prot… 40       P01374
```
