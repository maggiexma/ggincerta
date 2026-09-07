# Format input and assign the "map" class

`duo()` and `duo_pixel()` create paired mapping objects that combine two
variables, record their names, and assign the bivariate/pixel class as
an attribute for use in aesthetic mappings.

## Usage

``` r
duo(v1, v2)

duo_pixel(estimate, error)
```

## Arguments

- v1, v2:

  Input variables for `duo()`.

- estimate, error:

  Input variables for `duo_pixel()` representing the point estimate and
  its uncertainty.

## Value

A list-like object containing pairs of values from the two variables,
with attributes storing the variable names and the class.

## Examples

``` r
value <- nc$value
sd <- nc$sd
res <- duo(value, sd)
res_pixel <- duo_pixel(value, sd)
class(res); class(res_pixel)
#> [1] "bivariate" "list"     
#> [1] "pixel" "list" 
attr(res, "vars"); attr(res_pixel, "vars")
#> [[1]]
#> value
#> 
#> [[2]]
#> sd
#> 
#> [[1]]
#> value
#> 
#> [[2]]
#> sd
#> 
```
