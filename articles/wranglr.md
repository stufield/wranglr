# Introduction to wranglr

The `wranglr` package contains general functions necessary to manipulate
and wrangle internal `R` representations of proteomic data into
convenient forms for analysis.

------------------------------------------------------------------------

## Useful functions in `wranglr`

### Transforming Data

- [`center_scale()`](https://stufield.github.io/wranglr/reference/center_scale.md)
- [`create_recipe()`](https://stufield.github.io/wranglr/reference/create_recipe.md)

``` r
scaled <- center_scale(mtcars)               # all numeric features
apply(feature_matrix(scaled), 2, mean) |> sum()  # mean = 0
#> [1] -1.64365e-15
apply(feature_matrix(scaled), 2, sd)             # sd = 1
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>    1    1    1    1    1    1    1    1    1    1    1

# `create_recipe()`
rcp <- create_recipe(mtcars, feat = c("mpg", "disp", "drat", "wt"))
rcp
#> ══ Pre-processing recipe ══════════════════════════════════════════════
#> 
#> ─── Training data:
#> ✓ Data containing 32 samples used in recipe
#> ✓ RFU features ( n = 4 ) will be processed by:
#> 
#> ─── Steps:
#> • log10-transformed      ✓
#> • Centered (mean = 0)    ✓
#> • Scaled (sd = 1)        ✓
#> ═══════════════════════════════════════════════════════════════════════
```

------------------------------------------------------------------------

### Imputing Data

- [`remove_outliers()`](https://stufield.github.io/wranglr/reference/remove_outliers.md)
- [`impute_outliers()`](https://stufield.github.io/wranglr/reference/impute_outliers.md)
- [`imputeNAs()`](https://stufield.github.io/wranglr/reference/imputeNAs.md)
- [`impute_predictors()`](https://stufield.github.io/wranglr/reference/impute_predictors.md)

``` r
# Identify outliers (`helpr::get_outliers()`)
x <- withr::with_seed(1, rnorm(10))   # normal
x <- c(x, 100)                        # add outlier
get_outliers(x)                       # get index of the outlier
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
#> -0.820468384118015 -0.626453810742332 -0.221405243260125 
#>                  1                  1                  2 
#>  0.183643324222082   1.59528080213779 
#>                  1                  1

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

- [`bind_intersect()`](https://stufield.github.io/wranglr/reference/bind.md)
- [`bind_union()`](https://stufield.github.io/wranglr/reference/bind.md)

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

### Refactor

- [`refactor_data()`](https://stufield.github.io/wranglr/reference/refactor_data.md)
- [`feature_matrix()`](https://stufield.github.io/wranglr/reference/feature_matrix.md)

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
- [`seq_lookup()`](https://stufield.github.io/wranglr/reference/anno.md)
- [`seqify()`](https://stufield.github.io/wranglr/reference/anno.md)

``` r
seqs <- withr::with_seed(101, sample(names(sample_df), 10))
seqs
#>  [1] "seq.4500.50" "seq.2654.19" "seq.4993.16" "seq.3074.6" 
#>  [5] "seq.4721.54" "seq.3516.60" "seq.3194.36" "seq.3197.70"
#>  [9] "seq.3448.13" "seq.4719.58"

# NAs for those analytes dropped from menu
seq_lookup(seqs)
#> # A tibble: 10 × 9
#>    seq    SeqId EntrezGeneSymbol Target TargetFullName Dilution UniProt
#>    <chr>  <chr> <chr>            <chr>  <chr>          <chr>    <chr>  
#>  1 seq.4… 4500… CLEC11A          SCGF-… Stem cell gro… 0.5%     Q9Y240 
#>  2 seq.2… 2654… TNFRSF1A         TNF s… Tumor necrosi… 20%      P19438 
#>  3 seq.4… 4993… GSTA3            GSTA3  Glutathione S… 20%      Q16772 
#>  4 seq.3… 3074… LBP              LBP    Lipopolysacch… 0.005%   P18428 
#>  5 seq.4… 4721… TFF3             TFF3   Trefoil facto… 0.5%     Q07654 
#>  6 seq.3… 3516… CXCL12           SDF-1  Stromal cell-… 20%      P48061 
#>  7 seq.3… 3194… GP6              GPVI   Platelet glyc… 0.5%     Q9HCN6 
#>  8 seq.3… 3197… IDE              IDE    Insulin-degra… 20%      P14735 
#>  9 seq.3… 3448… INSR             IR     Insulin recep… 20%      P06213 
#> 10 seq.4… 4719… PDIA3            Prote… Protein disul… 0.5%     P30101 
#> # ℹ 2 more variables: List <chr>, Reason <chr>

seqify(seqs)
#> ══ SeqId Lookup ═══════════════════════════════════════════════════════
#>   SeqId-Feature     GeneID       Target                                                   List     Reason
#> ───────────────────────────────────────────────────────────────────────
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
#>    seq         SeqId   EntrezGeneSymbol Target           TargetFullName
#>    <chr>       <chr>   <chr>            <chr>            <chr>         
#>  1 seq.4500.50 4500-50 CLEC11A          SCGF-alpha       Stem Cell Gro…
#>  2 seq.2654.19 2654-19 TNFRSF1A         TNF sR-I         Tumor necrosi…
#>  3 seq.4993.16 4993-16 GSTA3            GSTA3            Glutathione S…
#>  4 seq.3074.6  3074-6  LBP              LBP              Lipopolysacch…
#>  5 seq.4721.54 4721-54 TFF3             TFF3             Trefoil facto…
#>  6 seq.3516.60 3516-60 CXCL12           SDF-1            Stromal cell-…
#>  7 seq.3194.36 3194-36 GP6              GPVI             Platelet glyc…
#>  8 seq.3197.70 3197-70 IDE              IDE              Insulin-degra…
#>  9 seq.3448.13 3448-13 INSR             IR               Insulin recep…
#> 10 seq.4719.58 4719-58 PDIA3            Protein disulfi… Protein disul…
```
