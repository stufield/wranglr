# Rename Elements of a List

See
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html),
but for lists. Rename list elements simply and efficiently. Missing
elements are unchanged and unmatched elements are ignored.

## Usage

``` r
# S3 method for class 'list'
rename(.data, ...)
```

## Arguments

- .data:

  A *named* `list` to be renamed.

- ...:

  For `rename()`:
  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Use `new_name = old_name` to rename selected variables.

  For
  [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html):
  additional arguments passed onto `.fn`.

## Value

A newly named list corresponding to the expressions passed to the `...`.

## Examples

``` r
foo <- list(
  a = 1:5,
  b = letters[1:10L],
  c = data.frame(set = 1:4, col = LETTERS[1:4L])
)

# Map all names
rename(foo, super = "b", awesome = "a", wicked = "c")
#> $awesome
#> [1] 1 2 3 4 5
#> 
#> $super
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $wicked
#>   set col
#> 1   1   A
#> 2   2   B
#> 3   3   C
#> 4   4   D
#> 

# Missing names are un-changed
rename(foo, super = "c", wicked = "b")
#> $a
#> [1] 1 2 3 4 5
#> 
#> $wicked
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $super
#>   set col
#> 1   1   A
#> 2   2   B
#> 3   3   C
#> 4   4   D
#> 

# Extra names are ignored
rename(foo, super = "b", wicked = "c", yellow = "d")
#> $a
#> [1] 1 2 3 4 5
#> 
#> $super
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $wicked
#>   set col
#> 1   1   A
#> 2   2   B
#> 3   3   C
#> 4   4   D
#> 

# The !!! is supported
key <- c(super = "b", awesome = "a", wicked = "c")
rename(foo, !!! key)
#> $awesome
#> [1] 1 2 3 4 5
#> 
#> $super
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $wicked
#>   set col
#> 1   1   A
#> 2   2   B
#> 3   3   C
#> 4   4   D
#> 
```
