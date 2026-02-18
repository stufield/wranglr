# Scale/transform Features (variables)

Scale features by a scalar reference value (named vector) match with the
features contained in `.data`. Columns without a corresponding reference
value are unmodified (with a warning).

## Usage

``` r
scale_features(.data, scale_vec)
```

## Arguments

- .data:

  A `data.frame` class object.

- scale_vec:

  A *named* vector of scalars, named by features in `.data`.

## Author

Stu Field

## Examples

``` r
ref  <- c(mpg = 10.0, wt = 0.1, drat = 1.0, disp = 0.0)
new  <- scale_features(mtcars, ref)
#> Warning: Missing scalar value for (7) features. They will not be transformed.
#> Please check the reference or its names.
new
#>                     mpg cyl disp  hp drat     wt  qsec vs am gear carb
#> Mazda RX4           210   6    0 110 3.90 0.2620 16.46  0  1    4    4
#> Mazda RX4 Wag       210   6    0 110 3.90 0.2875 17.02  0  1    4    4
#> Datsun 710          228   4    0  93 3.85 0.2320 18.61  1  1    4    1
#> Hornet 4 Drive      214   6    0 110 3.08 0.3215 19.44  1  0    3    1
#> Hornet Sportabout   187   8    0 175 3.15 0.3440 17.02  0  0    3    2
#> Valiant             181   6    0 105 2.76 0.3460 20.22  1  0    3    1
#> Duster 360          143   8    0 245 3.21 0.3570 15.84  0  0    3    4
#> Merc 240D           244   4    0  62 3.69 0.3190 20.00  1  0    4    2
#> Merc 230            228   4    0  95 3.92 0.3150 22.90  1  0    4    2
#> Merc 280            192   6    0 123 3.92 0.3440 18.30  1  0    4    4
#> Merc 280C           178   6    0 123 3.92 0.3440 18.90  1  0    4    4
#> Merc 450SE          164   8    0 180 3.07 0.4070 17.40  0  0    3    3
#> Merc 450SL          173   8    0 180 3.07 0.3730 17.60  0  0    3    3
#> Merc 450SLC         152   8    0 180 3.07 0.3780 18.00  0  0    3    3
#> Cadillac Fleetwood  104   8    0 205 2.93 0.5250 17.98  0  0    3    4
#> Lincoln Continental 104   8    0 215 3.00 0.5424 17.82  0  0    3    4
#> Chrysler Imperial   147   8    0 230 3.23 0.5345 17.42  0  0    3    4
#> Fiat 128            324   4    0  66 4.08 0.2200 19.47  1  1    4    1
#> Honda Civic         304   4    0  52 4.93 0.1615 18.52  1  1    4    2
#> Toyota Corolla      339   4    0  65 4.22 0.1835 19.90  1  1    4    1
#> Toyota Corona       215   4    0  97 3.70 0.2465 20.01  1  0    3    1
#> Dodge Challenger    155   8    0 150 2.76 0.3520 16.87  0  0    3    2
#> AMC Javelin         152   8    0 150 3.15 0.3435 17.30  0  0    3    2
#> Camaro Z28          133   8    0 245 3.73 0.3840 15.41  0  0    3    4
#> Pontiac Firebird    192   8    0 175 3.08 0.3845 17.05  0  0    3    2
#> Fiat X1-9           273   4    0  66 4.08 0.1935 18.90  1  1    4    1
#> Porsche 914-2       260   4    0  91 4.43 0.2140 16.70  0  1    5    2
#> Lotus Europa        304   4    0 113 3.77 0.1513 16.90  1  1    5    2
#> Ford Pantera L      158   8    0 264 4.22 0.3170 14.50  0  1    5    4
#> Ferrari Dino        197   6    0 175 3.62 0.2770 15.50  0  1    5    6
#> Maserati Bora       150   8    0 335 3.54 0.3570 14.60  0  1    5    8
#> Volvo 142E          214   4    0 109 4.11 0.2780 18.60  1  1    4    2
```
