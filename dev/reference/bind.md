# Vertically Combine Data Frames by Intersect

For `bind_intersect()`: [`rbind()`](https://rdrr.io/r/base/cbind.html)
is used to vertically **combine** data frames based on the *intersect*
of their column names. This creates *fewer* columns than the original
data, or at best the same number of columns. The resulting data frame
has the dimensions:

- rows: `nrow(df1) + nrow(df2) + ... + nrow(df_n)`

- cols: `intersect(names(...))`

For `bind_union()`: [`rbind()`](https://rdrr.io/r/base/cbind.html) is
used to vertically **merge** data frames based on the *union* of their
column names. This creates columns of `NAs` for the rows of a data frame
with non-overlapping column names. The resulting data frame has the
dimensions:

- rows: `nrow(df1) + nrow(df2) + ... + nrow(df_n)`

- cols: `union(names(...))`

## Usage

``` r
bind_intersect(...)

bind_union(...)
```

## Arguments

- ...:

  Data frames to combine. Can also be a *list* of data frames to
  combine.

## Value

A single data frame with the total number of rows =
`sum(sapply(..., nrow))`.

## Details

Incidentally, the default behavior of
[`rbind()`](https://rdrr.io/r/base/cbind.html) reorders the columns
correctly, but will only do so if their intersect matches.

## Note

For `bind_intersect()`, columns are combined on their *intersect* only.

For `bind_union()`, the ordering of the rows correspond to the order
they are supplied.

## See also

[`Reduce()`](https://rdrr.io/r/base/funprog.html),
[`rbind()`](https://rdrr.io/r/base/cbind.html),
[`generics::intersect()`](https://generics.r-lib.org/reference/setops.html)

[`generics::union()`](https://generics.r-lib.org/reference/setops.html)

## Author

Stu Field

## Examples

``` r
# For `bind_intersect()`
spl <- split(mtcars, mtcars$cyl) |> unname()
foo <- mapply(spl, -c(11, 10, 9), FUN = function(x, y) x[, y], SIMPLIFY = FALSE)
sapply(spl, names)
#>       [,1]   [,2]   [,3]  
#>  [1,] "mpg"  "mpg"  "mpg" 
#>  [2,] "cyl"  "cyl"  "cyl" 
#>  [3,] "disp" "disp" "disp"
#>  [4,] "hp"   "hp"   "hp"  
#>  [5,] "drat" "drat" "drat"
#>  [6,] "wt"   "wt"   "wt"  
#>  [7,] "qsec" "qsec" "qsec"
#>  [8,] "vs"   "vs"   "vs"  
#>  [9,] "am"   "am"   "am"  
#> [10,] "gear" "gear" "gear"
#> [11,] "carb" "carb" "carb"
sapply(spl, ncol)
#> [1] 11 11 11

# Pass a list
bind_intersect(spl)
#>                        data  mpg cyl  disp  hp drat    wt  qsec vs am
#> Datsun 710          data_01 22.8   4 108.0  93 3.85 2.320 18.61  1  1
#> Merc 240D           data_01 24.4   4 146.7  62 3.69 3.190 20.00  1  0
#> Merc 230            data_01 22.8   4 140.8  95 3.92 3.150 22.90  1  0
#> Fiat 128            data_01 32.4   4  78.7  66 4.08 2.200 19.47  1  1
#> Honda Civic         data_01 30.4   4  75.7  52 4.93 1.615 18.52  1  1
#> Toyota Corolla      data_01 33.9   4  71.1  65 4.22 1.835 19.90  1  1
#> Toyota Corona       data_01 21.5   4 120.1  97 3.70 2.465 20.01  1  0
#> Fiat X1-9           data_01 27.3   4  79.0  66 4.08 1.935 18.90  1  1
#> Porsche 914-2       data_01 26.0   4 120.3  91 4.43 2.140 16.70  0  1
#> Lotus Europa        data_01 30.4   4  95.1 113 3.77 1.513 16.90  1  1
#> Volvo 142E          data_01 21.4   4 121.0 109 4.11 2.780 18.60  1  1
#> Mazda RX4           data_02 21.0   6 160.0 110 3.90 2.620 16.46  0  1
#> Mazda RX4 Wag       data_02 21.0   6 160.0 110 3.90 2.875 17.02  0  1
#> Hornet 4 Drive      data_02 21.4   6 258.0 110 3.08 3.215 19.44  1  0
#> Valiant             data_02 18.1   6 225.0 105 2.76 3.460 20.22  1  0
#> Merc 280            data_02 19.2   6 167.6 123 3.92 3.440 18.30  1  0
#> Merc 280C           data_02 17.8   6 167.6 123 3.92 3.440 18.90  1  0
#> Ferrari Dino        data_02 19.7   6 145.0 175 3.62 2.770 15.50  0  1
#> Hornet Sportabout   data_03 18.7   8 360.0 175 3.15 3.440 17.02  0  0
#> Duster 360          data_03 14.3   8 360.0 245 3.21 3.570 15.84  0  0
#> Merc 450SE          data_03 16.4   8 275.8 180 3.07 4.070 17.40  0  0
#> Merc 450SL          data_03 17.3   8 275.8 180 3.07 3.730 17.60  0  0
#> Merc 450SLC         data_03 15.2   8 275.8 180 3.07 3.780 18.00  0  0
#> Cadillac Fleetwood  data_03 10.4   8 472.0 205 2.93 5.250 17.98  0  0
#> Lincoln Continental data_03 10.4   8 460.0 215 3.00 5.424 17.82  0  0
#> Chrysler Imperial   data_03 14.7   8 440.0 230 3.23 5.345 17.42  0  0
#> Dodge Challenger    data_03 15.5   8 318.0 150 2.76 3.520 16.87  0  0
#> AMC Javelin         data_03 15.2   8 304.0 150 3.15 3.435 17.30  0  0
#> Camaro Z28          data_03 13.3   8 350.0 245 3.73 3.840 15.41  0  0
#> Pontiac Firebird    data_03 19.2   8 400.0 175 3.08 3.845 17.05  0  0
#> Ford Pantera L      data_03 15.8   8 351.0 264 4.22 3.170 14.50  0  1
#> Maserati Bora       data_03 15.0   8 301.0 335 3.54 3.570 14.60  0  1
#>                     gear carb
#> Datsun 710             4    1
#> Merc 240D              4    2
#> Merc 230               4    2
#> Fiat 128               4    1
#> Honda Civic            4    2
#> Toyota Corolla         4    1
#> Toyota Corona          3    1
#> Fiat X1-9              4    1
#> Porsche 914-2          5    2
#> Lotus Europa           5    2
#> Volvo 142E             4    2
#> Mazda RX4              4    4
#> Mazda RX4 Wag          4    4
#> Hornet 4 Drive         3    1
#> Valiant                3    1
#> Merc 280               4    4
#> Merc 280C              4    4
#> Ferrari Dino           5    6
#> Hornet Sportabout      3    2
#> Duster 360             3    4
#> Merc 450SE             3    3
#> Merc 450SL             3    3
#> Merc 450SLC            3    3
#> Cadillac Fleetwood     3    4
#> Lincoln Continental    3    4
#> Chrysler Imperial      3    4
#> Dodge Challenger       3    2
#> AMC Javelin            3    2
#> Camaro Z28             3    4
#> Pontiac Firebird       3    2
#> Ford Pantera L         5    4
#> Maserati Bora          5    8

# Can pass either list or via '...'
identical(bind_intersect(spl), bind_intersect(spl[[1L]], spl[[2L]], spl[[3L]]))
#> [1] TRUE

# Passing a *named* list adds `data` column with those names
names(spl) <- letters[1:3L]
bind_intersect(spl)
#>                     data  mpg cyl  disp  hp drat    wt  qsec vs am
#> Datsun 710             a 22.8   4 108.0  93 3.85 2.320 18.61  1  1
#> Merc 240D              a 24.4   4 146.7  62 3.69 3.190 20.00  1  0
#> Merc 230               a 22.8   4 140.8  95 3.92 3.150 22.90  1  0
#> Fiat 128               a 32.4   4  78.7  66 4.08 2.200 19.47  1  1
#> Honda Civic            a 30.4   4  75.7  52 4.93 1.615 18.52  1  1
#> Toyota Corolla         a 33.9   4  71.1  65 4.22 1.835 19.90  1  1
#> Toyota Corona          a 21.5   4 120.1  97 3.70 2.465 20.01  1  0
#> Fiat X1-9              a 27.3   4  79.0  66 4.08 1.935 18.90  1  1
#> Porsche 914-2          a 26.0   4 120.3  91 4.43 2.140 16.70  0  1
#> Lotus Europa           a 30.4   4  95.1 113 3.77 1.513 16.90  1  1
#> Volvo 142E             a 21.4   4 121.0 109 4.11 2.780 18.60  1  1
#> Mazda RX4              b 21.0   6 160.0 110 3.90 2.620 16.46  0  1
#> Mazda RX4 Wag          b 21.0   6 160.0 110 3.90 2.875 17.02  0  1
#> Hornet 4 Drive         b 21.4   6 258.0 110 3.08 3.215 19.44  1  0
#> Valiant                b 18.1   6 225.0 105 2.76 3.460 20.22  1  0
#> Merc 280               b 19.2   6 167.6 123 3.92 3.440 18.30  1  0
#> Merc 280C              b 17.8   6 167.6 123 3.92 3.440 18.90  1  0
#> Ferrari Dino           b 19.7   6 145.0 175 3.62 2.770 15.50  0  1
#> Hornet Sportabout      c 18.7   8 360.0 175 3.15 3.440 17.02  0  0
#> Duster 360             c 14.3   8 360.0 245 3.21 3.570 15.84  0  0
#> Merc 450SE             c 16.4   8 275.8 180 3.07 4.070 17.40  0  0
#> Merc 450SL             c 17.3   8 275.8 180 3.07 3.730 17.60  0  0
#> Merc 450SLC            c 15.2   8 275.8 180 3.07 3.780 18.00  0  0
#> Cadillac Fleetwood     c 10.4   8 472.0 205 2.93 5.250 17.98  0  0
#> Lincoln Continental    c 10.4   8 460.0 215 3.00 5.424 17.82  0  0
#> Chrysler Imperial      c 14.7   8 440.0 230 3.23 5.345 17.42  0  0
#> Dodge Challenger       c 15.5   8 318.0 150 2.76 3.520 16.87  0  0
#> AMC Javelin            c 15.2   8 304.0 150 3.15 3.435 17.30  0  0
#> Camaro Z28             c 13.3   8 350.0 245 3.73 3.840 15.41  0  0
#> Pontiac Firebird       c 19.2   8 400.0 175 3.08 3.845 17.05  0  0
#> Ford Pantera L         c 15.8   8 351.0 264 4.22 3.170 14.50  0  1
#> Maserati Bora          c 15.0   8 301.0 335 3.54 3.570 14.60  0  1
#>                     gear carb
#> Datsun 710             4    1
#> Merc 240D              4    2
#> Merc 230               4    2
#> Fiat 128               4    1
#> Honda Civic            4    2
#> Toyota Corolla         4    1
#> Toyota Corona          3    1
#> Fiat X1-9              4    1
#> Porsche 914-2          5    2
#> Lotus Europa           5    2
#> Volvo 142E             4    2
#> Mazda RX4              4    4
#> Mazda RX4 Wag          4    4
#> Hornet 4 Drive         3    1
#> Valiant                3    1
#> Merc 280               4    4
#> Merc 280C              4    4
#> Ferrari Dino           5    6
#> Hornet Sportabout      3    2
#> Duster 360             3    4
#> Merc 450SE             3    3
#> Merc 450SL             3    3
#> Merc 450SLC            3    3
#> Cadillac Fleetwood     3    4
#> Lincoln Continental    3    4
#> Chrysler Imperial      3    4
#> Dodge Challenger       3    2
#> AMC Javelin            3    2
#> Camaro Z28             3    4
#> Pontiac Firebird       3    2
#> Ford Pantera L         5    4
#> Maserati Bora          5    8

# For `bind_union()`
bind_union(spl)
#>                     data  mpg cyl  disp  hp drat    wt  qsec vs am
#> Datsun 710             a 22.8   4 108.0  93 3.85 2.320 18.61  1  1
#> Merc 240D              a 24.4   4 146.7  62 3.69 3.190 20.00  1  0
#> Merc 230               a 22.8   4 140.8  95 3.92 3.150 22.90  1  0
#> Fiat 128               a 32.4   4  78.7  66 4.08 2.200 19.47  1  1
#> Honda Civic            a 30.4   4  75.7  52 4.93 1.615 18.52  1  1
#> Toyota Corolla         a 33.9   4  71.1  65 4.22 1.835 19.90  1  1
#> Toyota Corona          a 21.5   4 120.1  97 3.70 2.465 20.01  1  0
#> Fiat X1-9              a 27.3   4  79.0  66 4.08 1.935 18.90  1  1
#> Porsche 914-2          a 26.0   4 120.3  91 4.43 2.140 16.70  0  1
#> Lotus Europa           a 30.4   4  95.1 113 3.77 1.513 16.90  1  1
#> Volvo 142E             a 21.4   4 121.0 109 4.11 2.780 18.60  1  1
#> Mazda RX4              b 21.0   6 160.0 110 3.90 2.620 16.46  0  1
#> Mazda RX4 Wag          b 21.0   6 160.0 110 3.90 2.875 17.02  0  1
#> Hornet 4 Drive         b 21.4   6 258.0 110 3.08 3.215 19.44  1  0
#> Valiant                b 18.1   6 225.0 105 2.76 3.460 20.22  1  0
#> Merc 280               b 19.2   6 167.6 123 3.92 3.440 18.30  1  0
#> Merc 280C              b 17.8   6 167.6 123 3.92 3.440 18.90  1  0
#> Ferrari Dino           b 19.7   6 145.0 175 3.62 2.770 15.50  0  1
#> Hornet Sportabout      c 18.7   8 360.0 175 3.15 3.440 17.02  0  0
#> Duster 360             c 14.3   8 360.0 245 3.21 3.570 15.84  0  0
#> Merc 450SE             c 16.4   8 275.8 180 3.07 4.070 17.40  0  0
#> Merc 450SL             c 17.3   8 275.8 180 3.07 3.730 17.60  0  0
#> Merc 450SLC            c 15.2   8 275.8 180 3.07 3.780 18.00  0  0
#> Cadillac Fleetwood     c 10.4   8 472.0 205 2.93 5.250 17.98  0  0
#> Lincoln Continental    c 10.4   8 460.0 215 3.00 5.424 17.82  0  0
#> Chrysler Imperial      c 14.7   8 440.0 230 3.23 5.345 17.42  0  0
#> Dodge Challenger       c 15.5   8 318.0 150 2.76 3.520 16.87  0  0
#> AMC Javelin            c 15.2   8 304.0 150 3.15 3.435 17.30  0  0
#> Camaro Z28             c 13.3   8 350.0 245 3.73 3.840 15.41  0  0
#> Pontiac Firebird       c 19.2   8 400.0 175 3.08 3.845 17.05  0  0
#> Ford Pantera L         c 15.8   8 351.0 264 4.22 3.170 14.50  0  1
#> Maserati Bora          c 15.0   8 301.0 335 3.54 3.570 14.60  0  1
#>                     gear carb
#> Datsun 710             4    1
#> Merc 240D              4    2
#> Merc 230               4    2
#> Fiat 128               4    1
#> Honda Civic            4    2
#> Toyota Corolla         4    1
#> Toyota Corona          3    1
#> Fiat X1-9              4    1
#> Porsche 914-2          5    2
#> Lotus Europa           5    2
#> Volvo 142E             4    2
#> Mazda RX4              4    4
#> Mazda RX4 Wag          4    4
#> Hornet 4 Drive         3    1
#> Valiant                3    1
#> Merc 280               4    4
#> Merc 280C              4    4
#> Ferrari Dino           5    6
#> Hornet Sportabout      3    2
#> Duster 360             3    4
#> Merc 450SE             3    3
#> Merc 450SL             3    3
#> Merc 450SLC            3    3
#> Cadillac Fleetwood     3    4
#> Lincoln Continental    3    4
#> Chrysler Imperial      3    4
#> Dodge Challenger       3    2
#> AMC Javelin            3    2
#> Camaro Z28             3    4
#> Pontiac Firebird       3    2
#> Ford Pantera L         5    4
#> Maserati Bora          5    8
bind_union(spl[[1L]], spl[[2L]])
#>                   data  mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Datsun 710     data_01 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Merc 240D      data_01 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Merc 230       data_01 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Fiat 128       data_01 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic    data_01 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla data_01 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Toyota Corona  data_01 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Fiat X1-9      data_01 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Porsche 914-2  data_01 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Lotus Europa   data_01 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Volvo 142E     data_01 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#> Mazda RX4      data_02 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag  data_02 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Hornet 4 Drive data_02 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Valiant        data_02 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Merc 280       data_02 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C      data_02 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Ferrari Dino   data_02 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#>                carb
#> Datsun 710        1
#> Merc 240D         2
#> Merc 230          2
#> Fiat 128          1
#> Honda Civic       2
#> Toyota Corolla    1
#> Toyota Corona     1
#> Fiat X1-9         1
#> Porsche 914-2     2
#> Lotus Europa      2
#> Volvo 142E        2
#> Mazda RX4         4
#> Mazda RX4 Wag     4
#> Hornet 4 Drive    1
#> Valiant           1
#> Merc 280          4
#> Merc 280C         4
#> Ferrari Dino      6
bind_union(spl[[1L]], spl[[2L]], spl[[3L]])
#>                        data  mpg cyl  disp  hp drat    wt  qsec vs am
#> Datsun 710          data_01 22.8   4 108.0  93 3.85 2.320 18.61  1  1
#> Merc 240D           data_01 24.4   4 146.7  62 3.69 3.190 20.00  1  0
#> Merc 230            data_01 22.8   4 140.8  95 3.92 3.150 22.90  1  0
#> Fiat 128            data_01 32.4   4  78.7  66 4.08 2.200 19.47  1  1
#> Honda Civic         data_01 30.4   4  75.7  52 4.93 1.615 18.52  1  1
#> Toyota Corolla      data_01 33.9   4  71.1  65 4.22 1.835 19.90  1  1
#> Toyota Corona       data_01 21.5   4 120.1  97 3.70 2.465 20.01  1  0
#> Fiat X1-9           data_01 27.3   4  79.0  66 4.08 1.935 18.90  1  1
#> Porsche 914-2       data_01 26.0   4 120.3  91 4.43 2.140 16.70  0  1
#> Lotus Europa        data_01 30.4   4  95.1 113 3.77 1.513 16.90  1  1
#> Volvo 142E          data_01 21.4   4 121.0 109 4.11 2.780 18.60  1  1
#> Mazda RX4           data_02 21.0   6 160.0 110 3.90 2.620 16.46  0  1
#> Mazda RX4 Wag       data_02 21.0   6 160.0 110 3.90 2.875 17.02  0  1
#> Hornet 4 Drive      data_02 21.4   6 258.0 110 3.08 3.215 19.44  1  0
#> Valiant             data_02 18.1   6 225.0 105 2.76 3.460 20.22  1  0
#> Merc 280            data_02 19.2   6 167.6 123 3.92 3.440 18.30  1  0
#> Merc 280C           data_02 17.8   6 167.6 123 3.92 3.440 18.90  1  0
#> Ferrari Dino        data_02 19.7   6 145.0 175 3.62 2.770 15.50  0  1
#> Hornet Sportabout   data_03 18.7   8 360.0 175 3.15 3.440 17.02  0  0
#> Duster 360          data_03 14.3   8 360.0 245 3.21 3.570 15.84  0  0
#> Merc 450SE          data_03 16.4   8 275.8 180 3.07 4.070 17.40  0  0
#> Merc 450SL          data_03 17.3   8 275.8 180 3.07 3.730 17.60  0  0
#> Merc 450SLC         data_03 15.2   8 275.8 180 3.07 3.780 18.00  0  0
#> Cadillac Fleetwood  data_03 10.4   8 472.0 205 2.93 5.250 17.98  0  0
#> Lincoln Continental data_03 10.4   8 460.0 215 3.00 5.424 17.82  0  0
#> Chrysler Imperial   data_03 14.7   8 440.0 230 3.23 5.345 17.42  0  0
#> Dodge Challenger    data_03 15.5   8 318.0 150 2.76 3.520 16.87  0  0
#> AMC Javelin         data_03 15.2   8 304.0 150 3.15 3.435 17.30  0  0
#> Camaro Z28          data_03 13.3   8 350.0 245 3.73 3.840 15.41  0  0
#> Pontiac Firebird    data_03 19.2   8 400.0 175 3.08 3.845 17.05  0  0
#> Ford Pantera L      data_03 15.8   8 351.0 264 4.22 3.170 14.50  0  1
#> Maserati Bora       data_03 15.0   8 301.0 335 3.54 3.570 14.60  0  1
#>                     gear carb
#> Datsun 710             4    1
#> Merc 240D              4    2
#> Merc 230               4    2
#> Fiat 128               4    1
#> Honda Civic            4    2
#> Toyota Corolla         4    1
#> Toyota Corona          3    1
#> Fiat X1-9              4    1
#> Porsche 914-2          5    2
#> Lotus Europa           5    2
#> Volvo 142E             4    2
#> Mazda RX4              4    4
#> Mazda RX4 Wag          4    4
#> Hornet 4 Drive         3    1
#> Valiant                3    1
#> Merc 280               4    4
#> Merc 280C              4    4
#> Ferrari Dino           5    6
#> Hornet Sportabout      3    2
#> Duster 360             3    4
#> Merc 450SE             3    3
#> Merc 450SL             3    3
#> Merc 450SLC            3    3
#> Cadillac Fleetwood     3    4
#> Lincoln Continental    3    4
#> Chrysler Imperial      3    4
#> Dodge Challenger       3    2
#> AMC Javelin            3    2
#> Camaro Z28             3    4
#> Pontiac Firebird       3    2
#> Ford Pantera L         5    4
#> Maserati Bora          5    8
```
