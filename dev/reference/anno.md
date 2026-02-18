# Protein Annotations

A group of functions that allow for the simple search and access of
protein annotation information based on an internal lookup table.

## Usage

``` r
get_anno()

grep_anno(pattern)

seq_lookup(seq, tbl = NULL)

seqify(seq)
```

## Arguments

- pattern:

  `character(1)`. Regular expression pattern to match.

- seq:

  `character(n)`. A vector of `SeqIds`, or strings *containing*
  `SeqId`s.

- tbl:

  A `tibble` containing annotation data.

## Value

`get_anno()`: a `tibble` of protein annotation information.

`grep_anno()`: a `tibble` of a subset of protein information
(annotations) corresponding to matching rows of in `pattern`.

`seq_lookup()`: a `tibble`, a subset of `tbl`, corresponding to the rows
whose `SeqIds` match the values in `seq`.

`seqify()`: a `wranglr_seq` class object.

## Details

Historical features may have been dropped from the menu may be absent
from the internal lookup table, resulting in `NA` for that row. In such
cases, they can be retrieved by explicitly passing an annotations table
corresponding to the original data prior to the menu change. The easiest
way to generate a "time-capsuled" annotations table: 1. via
`attr(data, "Col.Meta")`

## Functions

- `get_anno()`: a convenient wrapper to easily retrieve the lookup table
  (`tibble`) of proteomic annotations keyed on `SeqId`.

- `grep_anno()`: pattern matches via regular expression of the internal
  "annotations" table (`tibble`). Matches are performed on *any*
  (`regexpr = "|"`) of the following:

  - `EntrezGeneSymbol`

  - `TargetFullName`

  - `Target`

  - `List`

  - `SeqId`

- `seq_lookup()`: conveniently looks up annotation keyed on a vector of
  `SeqId`s. The internal lookup dictionary is matched on values in the
  `SeqId` column of annotations, which accesses a static internal
  object. Alternatively, the user can explicitly pass an annotations
  table of of their choice.

- `seqify()`: converts to `wranglr_seq` object, primarily to dispatch
  the S3 print method during interactive use.

## Author

Stu Field

## Examples

``` r
get_anno()
#> # A tibble: 11,083 × 44
#>    SeqId    TargetFullName  EntrezGeneSymbol ApparentKdM UniProt SomaId
#>    <chr>    <chr>           <chr>                  <dbl> <chr>   <chr> 
#>  1 10000-28 Beta-crystalli… CRYBB2              1.24e-10 P43320  SL019…
#>  2 10001-7  RAF proto-onco… RAF1                1.18e-11 P04049  SL002…
#>  3 10003-15 Zinc finger pr… ZNF41               4.33e-11 P51814  SL019…
#>  4 10006-25 ETS domain-con… ELK1                3.37e-12 P19419  SL019…
#>  5 10008-43 Guanylyl cycla… GUCA1A              5.26e-11 P43080  SL019…
#>  6 10010-10 Beclin-1        BECN1               3.94e-10 Q14457  SL014…
#>  7 10011-65 Inositol polyp… OCRL                7.25e-11 Q01968  SL019…
#>  8 10012-5  SAM pointed do… SPDEF               1.35e-10 O95238  SL014…
#>  9 10013-34 Internal Use O… Igh                NA        Q99LC4  SL025…
#> 10 10014-31 Zinc finger pr… SNAI2               1.84e-11 O43623  SL007…
#> # ℹ 11,073 more rows
#> # ℹ 38 more variables: Organism <chr>, Dilution <chr>, Target <chr>,
#> #   Type <chr>, List <chr>, Reason <chr>, `LoDB (RFU)` <dbl>,
#> #   `Signal To Noise Plasma` <dbl>, `Intra-Plate CV Plasma` <dbl>,
#> #   `Inter-Plate CV Plasma` <dbl>, `Total CV Plasma` <dbl>,
#> #   `Correlation Plasma` <dbl>, `F-Statistic Plasma` <dbl>,
#> #   `Lower 95% Normal Plasma (RFU)` <dbl>, …

grep_anno("^MMP")     # match EntrezGeneSymbols
#> # A tibble: 24 × 44
#>    SeqId    TargetFullName  EntrezGeneSymbol ApparentKdM UniProt SomaId
#>    <chr>    <chr>           <chr>                  <dbl> <chr>   <chr> 
#>  1 10479-18 Stromelysin-2   MMP10               2.31e-11 P09238  SL000…
#>  2 15419-15 Matrix metallo… MMP20               1.36e-11 O60882  SL012…
#>  3 2579-17  Matrix metallo… MMP9                2.89e-11 P14780  SL000…
#>  4 25932-61 Matrix metallo… MMP23B              2.24e-11 O75900  SL024…
#>  5 2788-55  Stromelysin-1   MMP3                4.53e-11 P08254  SL000…
#>  6 2789-26  Matrilysin      MMP7                7.89e-12 P09237  SL000…
#>  7 2838-53  Matrix metallo… MMP17               1.88e-10 Q9ULZ9  SL003…
#>  8 2954-56  Neutrophil col… MMP8                1.36e-11 P22894  SL000…
#>  9 31177-67 Matrix metallo… MMP15               3.18e-11 P51511  SL003…
#> 10 33871-7  Matrix metallo… MMP15               1.66e-11 P51511  SL003…
#> # ℹ 14 more rows
#> # ℹ 38 more variables: Organism <chr>, Dilution <chr>, Target <chr>,
#> #   Type <chr>, List <chr>, Reason <chr>, `LoDB (RFU)` <dbl>,
#> #   `Signal To Noise Plasma` <dbl>, `Intra-Plate CV Plasma` <dbl>,
#> #   `Inter-Plate CV Plasma` <dbl>, `Total CV Plasma` <dbl>,
#> #   `Correlation Plasma` <dbl>, `F-Statistic Plasma` <dbl>,
#> #   `Lower 95% Normal Plasma (RFU)` <dbl>, …
grep_anno("^29")      # match SeqId
#> # A tibble: 476 × 44
#>    SeqId     TargetFullName EntrezGeneSymbol ApparentKdM UniProt SomaId
#>    <chr>     <chr>          <chr>                  <dbl> <chr>   <chr> 
#>  1 2900-53   C-C motif che… CCL14               6.07e-11 Q16627  SL003…
#>  2 2906-55   Interleukin-4  IL4                 1.15e-12 P05112  SL000…
#>  3 29065-61  Cyclin-depend… CDK19               1.51e-10 Q9BWU1  SL021…
#>  4 29066-34  Myristoylated… MARCKS              1.42e-11 P29966  SL008…
#>  5 29067-43  Protein MICAL… MICAL2              3.63e-12 O94851  SL014…
#>  6 29068-2   Melanoma-asso… MAGEB6              4.55e-11 Q8N7X4  SL013…
#>  7 29069-5   Tubulin beta-… TUBB4A              4.69e-11 P04350  SL007…
#>  8 29070-6   TFAR15         PDCD10              1.18e-11 Q9BUL8  SL003…
#>  9 29071-219 UGT 1A10       UGT1A10             3.19e-10 Q9HAW8  SL000…
#> 10 29072-84  Calcium/calmo… CAMK1G              1.81e-10 Q96NX5  SL021…
#> # ℹ 466 more rows
#> # ℹ 38 more variables: Organism <chr>, Dilution <chr>, Target <chr>,
#> #   Type <chr>, List <chr>, Reason <chr>, `LoDB (RFU)` <dbl>,
#> #   `Signal To Noise Plasma` <dbl>, `Intra-Plate CV Plasma` <dbl>,
#> #   `Inter-Plate CV Plasma` <dbl>, `Total CV Plasma` <dbl>,
#> #   `Correlation Plasma` <dbl>, `F-Statistic Plasma` <dbl>,
#> #   `Lower 95% Normal Plasma (RFU)` <dbl>, …
grep_anno("^black")   # Blacklisted
#> # A tibble: 363 × 44
#>    SeqId    TargetFullName  EntrezGeneSymbol ApparentKdM UniProt SomaId
#>    <chr>    <chr>           <chr>                  <dbl> <chr>   <chr> 
#>  1 10013-34 Internal Use O… Igh                       NA Q99LC4  SL025…
#>  2 10021-1  Internal Use O… Igh                       NA Q99LC4  SL025…
#>  3 10034-16 Internal Use O… Igh                       NA Q99LC4  SL025…
#>  4 10416-79 Internal Use O… Igh                       NA Q99LC4  SL025…
#>  5 10427-2  Internal Use O… Igh                       NA Q99LC4  SL025…
#>  6 10452-24 Internal Use O… Igh                       NA Q99LC4  SL025…
#>  7 10461-57 Internal Use O… Igh                       NA Q99LC4  SL025…
#>  8 10467-58 Internal Use O… Igh                       NA Q99LC4  SL025…
#>  9 10471-25 Internal Use O… Igh                       NA Q99LC4  SL025…
#> 10 10476-23 Internal Use O… Igh                       NA Q99LC4  SL025…
#> # ℹ 353 more rows
#> # ℹ 38 more variables: Organism <chr>, Dilution <chr>, Target <chr>,
#> #   Type <chr>, List <chr>, Reason <chr>, `LoDB (RFU)` <dbl>,
#> #   `Signal To Noise Plasma` <dbl>, `Intra-Plate CV Plasma` <dbl>,
#> #   `Inter-Plate CV Plasma` <dbl>, `Total CV Plasma` <dbl>,
#> #   `Correlation Plasma` <dbl>, `F-Statistic Plasma` <dbl>,
#> #   `Lower 95% Normal Plasma (RFU)` <dbl>, …

svec <- c("seq.2981.9", "seq.5073.30", "seq.4429.51", "seq.2447.7")
svec
#> [1] "seq.2981.9"  "seq.5073.30" "seq.4429.51" "seq.2447.7" 

seq_lookup(svec)
#> # A tibble: 4 × 9
#>   seq     SeqId EntrezGeneSymbol Target TargetFullName Dilution UniProt
#>   <chr>   <chr> <chr>            <chr>  <chr>          <chr>    <chr>  
#> 1 seq.29… 2981… ESAM             ESAM   Endothelial c… 0.5%     Q96AP7 
#> 2 seq.50… 5073… NA               NA     NA             NA       NA     
#> 3 seq.44… 4429… CHST6            CHST6  Carbohydrate … 20%      Q9GZX3 
#> 4 seq.24… 2447… PLA2G2E          GIIE   Group IIE sec… 20%      Q9NZK7 
#> # ℹ 2 more variables: List <chr>, Reason <chr>

# print method for class `wranglr_seq`
seqify(svec) |> class()
#> [1] "wranglr_seq" "character"  

# works with seq.xxxx.xx format
seqify(svec)
#> ══ SeqId Lookup ═══════════════════════════════════════════════════════
#>   SeqId-Feature     GeneID      Target                                           List     Reason   
#> ───────────────────────────────────────────────────────────────────────
#> ▶ seq.2981.9     ❯  ESAM     ❯  Endothelial cell-selective adhesion molecule  ❯  NA    ❯  NA    
#> ▶ seq.5073.30    ❯  NA       ❯  NA                                            ❯  NA    ❯  NA    
#> ▶ seq.4429.51    ❯  CHST6    ❯  Carbohydrate sulfotransferase 6               ❯        ❯        
#> ▶ seq.2447.7     ❯  PLA2G2E  ❯  Group IIE secretory phospholipase A2          ❯        ❯        

# also works with pure SeqIds
vec <- sub("\\.", "-", sub("^seq\\.", "", svec))
vec
#> [1] "2981-9"  "5073-30" "4429-51" "2447-7" 

seqify(vec)
#> ══ SeqId Lookup ═══════════════════════════════════════════════════════
#>   SeqId-Feature     GeneID      Target                                           List     Reason   
#> ───────────────────────────────────────────────────────────────────────
#> ▶ 2981-9         ❯  ESAM     ❯  Endothelial cell-selective adhesion molecule  ❯  NA    ❯  NA    
#> ▶ 5073-30        ❯  NA       ❯  NA                                            ❯  NA    ❯  NA    
#> ▶ 4429-51        ❯  CHST6    ❯  Carbohydrate sulfotransferase 6               ❯        ❯        
#> ▶ 2447-7         ❯  PLA2G2E  ❯  Group IIE secretory phospholipase A2          ❯        ❯        
```
