
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `splyr` package

<!-- badges: start -->

[![build](https://img.shields.io/badge/build-passing-success.svg?logo=travis)](http://bitbucket.sladmin.com/projects/SV/repos/somaplyr/commits)
![coverage](https://img.shields.io/badge/coverage-74.1%25-yellow.svg?style=flat&logo=codecov)
![lint](https://img.shields.io/badge/lints-0-success.svg?style=flat&logo=gitlab)
[![pkgdown](https://img.shields.io/badge/pkgdown-_-critical.svg?logo=semantic-web&logoColor=red)](https://bitbucket.sladmin.com/pages/SV/somaplyr/bb-pkgdown/browse/index.html)
[![License:
GPL-3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

The `splyr` package contains the general functions necessary to
manipulate internal `R` representations of `*.adat` files and wrangle
those data into convenient form for analysis.

-----

## Installation

``` r
remotes::install_github("splyr")
```

-----

## Usage

To load `splyr` simply make a call to `library()` as usual:

``` r
library(splyr)
```

## Help summary of the package

``` r
library(help = splyr)
```

-----

## Useful functions in `splyr`

### Transforming Data

  - `centerScaleData()`
  - `somaRecipe()`

<!-- end list -->

``` r
scaled <- centerScaleData(sim_test_data)
apply(stripMeta(scaled), 2, mean) |> sum()  # mean = 0
#> [1] -1.049005e-15
apply(stripMeta(scaled), 2, sd)             # sd = 1
#> seq.2802.68 seq.9251.29 seq.1942.70 seq.5751.80 seq.9608.12 seq.3459.49 
#>           1           1           1           1           1           1 
#> seq.3865.56 seq.3363.21 seq.4487.88 seq.5994.84 seq.9011.72 seq.2902.23 
#>           1           1           1           1           1           1 
#> seq.2260.48 seq.4936.96 seq.2277.95 seq.2953.31 seq.3032.11  seq.4330.4 
#>           1           1           1           1           1           1 
#> seq.4914.10  seq.3896.5  seq.5002.7  seq.3476.4 seq.1130.49 seq.6356.60 
#>           1           1           1           1           1           1 
#> seq.4579.40 seq.8344.24 seq.8441.53 seq.9360.55  seq.7841.8 seq.8142.63 
#>           1           1           1           1           1           1 
#> seq.4461.56 seq.9297.97 seq.9396.38 seq.3300.26 seq.2772.14 seq.6615.18 
#>           1           1           1           1           1           1 
#> seq.8797.98 seq.9879.88 seq.8993.16 seq.9373.82 
#>           1           1           1           1

# `somaRecipe()`
rcp <- somaRecipe(sim_test_data)
rcp
#> ══ SomaScan pre-processing recipe ══════════════════════════════════════════════
#> 
#> ─── Training data:
#> ✓ Data containing 100 samples used in recipe
#> ✓ RFU features ( n = 40 ) will be processed by:
#> 
#> ─── Steps:
#> • Bridging              ✖
#> • log10-transformed     ✓
#> • Centered (mean = 0)   ✓
#> • Scaled (sd = 1)       ✓
#> 
#> ════════════════════════════════════════════════════════════════════════════════
```

-----

### Imputing Data

  - `imputeOutliers()`, `getOutliers()` (`SomaGlobals`)
  - `imputeNAs()`
  - `imputePredictors()`

<!-- end list -->

``` r
# Outliers
x <- withr::with_seed(1, rnorm(10))   # normal
x <- c(x, 100)                        # add outlier
getOutliers(x)                        # index of the outlier
#> [1] 11

# parameters stored in attributes (parametric only)
attributes(getOutliers(x, type = "para"))
#> $mu
#> [1] 0.1157106
#> 
#> $sigma
#> [1] 0.9440787
#> 
#> $crit
#> [1] -2.716526  2.947947

# Impute 11th value
imputeOutliers(x)
#>  [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078 -0.8204684
#>  [7]  0.4874291  0.7383247  0.5757814 -0.3053884  2.9479467

# Impute NAs
x <- withr::with_seed(1, rnorm(6))
x[ c(3, 5) ] <- NA
median(x, na.rm = TRUE)
#> [1] -0.2214052

imputeNAs(x)
#> [1] -0.6264538  0.1836433 -0.2214052  1.5952808 -0.2214052 -0.8204684

table(imputeNAs(x))
#> 
#> -0.820468384118015 -0.626453810742332 -0.221405243260125  0.183643324222082 
#>                  1                  1                  2                  1 
#>   1.59528080213779 
#>                  1

# Predictors
x   <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = c(1.23, 4.56, 7.89))
tbl <- tibble::tribble(
  ~ AptName,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
    "a",         NA,        NA,           NA,         NA,
    "b",         5,         5,            0,          1,
    "c",         9,         7,            7.1,        7.1
)
imputePredictors(x, tbl)
#>   a b   c    d
#> 1 1 4 7.1 1.23
#> 2 2 5 8.0 4.56
#> 3 3 5 9.0 7.89
```

-----

### Shaping Data

  - `pivotLong()`

<!-- end list -->

``` r
widelong <- tibble::tibble(
  subject_id    = c(1561L, 1561L, 1561L, 3358L, 3358L, 3358L),
  ie            = c("IE1", "IE2", "IE3", "IE1", "IE2", "IE3"),
  clin_var1_ie1 = c(93L, 93L, 93L, 56L, 56L, 56L),
  clin_var1_ie2 = c(84L, 84L, 84L, 79L, 79L, 79L),
  clin_var1_ie3 = c(89L, 89L, 89L, 68L, 68L, 68L),
  clin_var2_ie1 = c(15L, 15L, 15L, 77L, 77L, 77L),
  clin_var2_ie2 = c(99L, 99L, 99L, 92L, 92L, 92L),
  clin_var2_ie3 = c(100L, 100L, 100L, 16L, 16L, 16L),
  clin_var3_ie1 = c("N", "N", "N", "N", "N", "N"),
  clin_var3_ie3 = c("N", "N", "N", "N", "N", "N"),
  seq.15529.33  = c(31498.5, 20108.1, 11905.1, 5726.6, 46357.8, 47059.8)
)
widelong
#> # A tibble: 6 × 11
#>   subject_id ie    clin_var1_ie1 clin_var1_ie2 clin_var1_ie3 clin_var2_ie1
#>        <int> <chr>         <int>         <int>         <int>         <int>
#> 1       1561 IE1              93            84            89            15
#> 2       1561 IE2              93            84            89            15
#> 3       1561 IE3              93            84            89            15
#> 4       3358 IE1              56            79            68            77
#> 5       3358 IE2              56            79            68            77
#> 6       3358 IE3              56            79            68            77
#> # ℹ 5 more variables: clin_var2_ie2 <int>, clin_var2_ie3 <int>,
#> #   clin_var3_ie1 <chr>, clin_var3_ie3 <chr>, seq.15529.33 <dbl>

# run with default arguments
pivotLong(widelong)
#> # A tibble: 6 × 6
#>   subject_id ie    clin_var1 clin_var2 clin_var3 seq.15529.33
#>        <int> <chr>     <int>     <int> <chr>            <dbl>
#> 1       1561 IE1          93        15 N               31498.
#> 2       1561 IE2          84        99 <NA>            20108.
#> 3       1561 IE3          89       100 N               11905.
#> 4       3358 IE1          56        77 N                5727.
#> 5       3358 IE2          79        92 <NA>            46358.
#> 6       3358 IE3          68        16 N               47060.
```

-----

### Binding Data

  - `rbindIntersect()`
  - `rbindUnion()`

<!-- end list -->

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

rbindIntersect(list_df)
#>   data a b
#> A    a 1 2
#> B    b 4 5
#> C    c 7 8

rbindUnion(list_df)
#>   data a b  c  d  e
#> A    a 1 2  3 NA NA
#> B    b 4 5 NA  6 NA
#> C    c 7 8 NA NA  9
```

-----

### Stripping Data

  - `refactorData()`
  - `stripMeta()`

<!-- end list -->

``` r
df <- data.frame(a = factor(c("a", "b")), b = 1:2)
foo <- df[df$a == "a", ]
foo
#>   a b
#> 1 a 1

levels(foo$a)   # 2 levels! "b" is a ghost level
#> [1] "a" "b"

bar <- refactorData(foo)
levels(bar$a)   # 1 level now
#> [1] "a"
```

-----

### `seq.XXXX` formats

  - `seqLookup()`
  - `seqify()`
  - `lookupAnnotations()`

<!-- end list -->

``` r
seqs <- withr::with_seed(101, sample(SomaDataIO::getAnalytes(sample.adat), 10L))
seqs
#>  [1] "seq.4545.53" "seq.2682.68" "seq.5004.69" "seq.3115.64" "seq.4801.13"
#>  [6] "seq.3554.24" "seq.3204.2"  "seq.3209.69" "seq.3469.74" "seq.4792.51"

# NAs for those analytes dropped from menu
seqLookup(seqs)
#> # A tibble: 10 × 10
#>    seq       SeqId EntrezGeneSymbol Target TargetFullName Type  Dilution UniProt
#>    <chr>     <chr> <chr>            <chr>  <chr>          <chr> <chr>    <chr>  
#>  1 seq.4545… 4545… DNAJC19          DnaJ … Mitochondrial… Prot… 20%      Q96DA6 
#>  2 seq.2682… 2682… HSPD1            HSP 60 60 kDa heat s… Prot… 20%      P10809 
#>  3 seq.5004… 5004… MAPK11           MK11   Mitogen-activ… Prot… 20%      Q15759 
#>  4 seq.3115… 3115… MAPK1            MK01   Mitogen-activ… Prot… 20%      P28482 
#>  5 seq.4801… 4801… LPO              PERL   Lactoperoxida… Prot… 20%      P22079 
#>  6 seq.3554… 3554… ADIPOQ           Adipo… Adiponectin    Prot… 0.005%   Q15848 
#>  7 seq.3204… 3204… LTA4H            LKHA4  Leukotriene A… Prot… 20%      P09960 
#>  8 seq.3209… 3209… MEPE             MEPE   Matrix extrac… Prot… 20%      Q9NQ76 
#>  9 seq.3469… 3469… RPS6KA3          RPS6K… Ribosomal pro… Prot… 20%      P51812 
#> 10 seq.4792… 4792… env              C34 g… gp41 C34 pept… Prot… 20%      Q70626 
#> # ℹ 2 more variables: List <chr>, Reason <chr>

seqify(seqs)
#> ══ SeqId Lookup ════════════════════════════════════════════════════════════════
#>   SeqId-AptName     GeneID      Target                                                            List          Reason        
#> ────────────────────────────────────────────────────────────────────────────────
#> ▶ seq.4545.53    ❯  DNAJC19  ❯  Mitochondrial import inner membrane translocase subunit TIM14  ❯             ❯             
#> ▶ seq.2682.68    ❯  HSPD1    ❯  60 kDa heat shock protein, mitochondrial                       ❯             ❯             
#> ▶ seq.5004.69    ❯  MAPK11   ❯  Mitogen-activated protein kinase 11                            ❯             ❯             
#> ▶ seq.3115.64    ❯  MAPK1    ❯  Mitogen-activated protein kinase 1                             ❯             ❯             
#> ▶ seq.4801.13    ❯  LPO      ❯  Lactoperoxidase                                                ❯             ❯             
#> ▶ seq.3554.24    ❯  ADIPOQ   ❯  Adiponectin                                                    ❯             ❯             
#> ▶ seq.3204.2     ❯  LTA4H    ❯  Leukotriene A-4 hydrolase                                      ❯             ❯             
#> ▶ seq.3209.69    ❯  MEPE     ❯  Matrix extracellular phosphoglycoprotein                       ❯             ❯             
#> ▶ seq.3469.74    ❯  RPS6KA3  ❯  Ribosomal protein S6 kinase alpha-3                            ❯             ❯             
#> ▶ seq.4792.51    ❯  env      ❯  gp41 C34 peptide, HIV                                          ❯  blacklist  ❯  Non-Protein

# Pass `apt.data` from appropriate ADAT
# to reconstitute those missing analytes
ad <- SomaDataIO::getAnalyteInfo(sample.adat)
seqLookup(seqs, ad)
#> # A tibble: 10 × 8
#>    seq       SeqId EntrezGeneSymbol Target TargetFullName Type  Dilution UniProt
#>    <chr>     <chr> <chr>            <chr>  <chr>          <chr> <chr>    <chr>  
#>  1 seq.4545… 4545… DNAJC19          DnaJ … Mitochondrial… Prot… 40       Q96DA6 
#>  2 seq.2682… 2682… HSPD1            HSP 60 60 kDa heat s… Prot… 40       P10809 
#>  3 seq.5004… 5004… MAPK11           MK11   Mitogen-activ… Prot… 40       Q15759 
#>  4 seq.3115… 3115… MAPK1            MK01   Mitogen-activ… Prot… 40       P28482 
#>  5 seq.4801… 4801… LPO              PERL   Lactoperoxida… Prot… 40       P22079 
#>  6 seq.3554… 3554… ADIPOQ           Adipo… Adiponectin    Prot… 0.005    Q15848 
#>  7 seq.3204… 3204… LTA4H            LKHA4  Leukotriene A… Prot… 40       P09960 
#>  8 seq.3209… 3209… MEPE             MEPE   Matrix extrac… Prot… 40       Q9NQ76 
#>  9 seq.3469… 3469… RPS6KA3          RPS6K… Ribosomal pro… Prot… 40       P51812 
#> 10 seq.4792… 4792… Human-virus      C34 g… gp41 C34 pept… Prot… 40       Q70626

# OR: on the fly
seqLookup(seqs, attributes(sample.adat)$Col.Meta)
#> # A tibble: 10 × 8
#>    seq       SeqId EntrezGeneSymbol Target TargetFullName Type  Dilution UniProt
#>    <chr>     <chr> <chr>            <chr>  <chr>          <chr> <chr>    <chr>  
#>  1 seq.4545… 4545… DNAJC19          DnaJ … Mitochondrial… Prot… 40       Q96DA6 
#>  2 seq.2682… 2682… HSPD1            HSP 60 60 kDa heat s… Prot… 40       P10809 
#>  3 seq.5004… 5004… MAPK11           MK11   Mitogen-activ… Prot… 40       Q15759 
#>  4 seq.3115… 3115… MAPK1            MK01   Mitogen-activ… Prot… 40       P28482 
#>  5 seq.4801… 4801… LPO              PERL   Lactoperoxida… Prot… 40       P22079 
#>  6 seq.3554… 3554… ADIPOQ           Adipo… Adiponectin    Prot… 0.005    Q15848 
#>  7 seq.3204… 3204… LTA4H            LKHA4  Leukotriene A… Prot… 40       P09960 
#>  8 seq.3209… 3209… MEPE             MEPE   Matrix extrac… Prot… 40       Q9NQ76 
#>  9 seq.3469… 3469… RPS6KA3          RPS6K… Ribosomal pro… Prot… 40       P51812 
#> 10 seq.4792… 4792… Human-virus      C34 g… gp41 C34 pept… Prot… 40       Q70626
```

-----

#### LICENSE
