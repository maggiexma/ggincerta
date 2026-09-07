# North Carolina SIDS data

The dataset `nc` is derived from the North Carolina shapefile (`nc.shp`)
included in the sf package. Two random variables, `value` and `sd`, have
been added for demonstration purposes.

Further details about the original data can be found in the [spdep
package vignette](https://r-spatial.github.io/spdep/articles/sids.html).

## Usage

``` r
nc
```

## Format

A `sf` object.

## Examples

``` r
head(nc)
#> Simple feature collection with 6 features and 18 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -81.74107 ymin: 36.07282 xmax: -75.77316 ymax: 36.58965
#> Geodetic CRS:  NAD27
#>    AREA PERIMETER CNTY_ CNTY_ID        NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
#> 1 0.114     1.442  1825    1825        Ashe 37009  37009        5  1091     1
#> 2 0.061     1.231  1827    1827   Alleghany 37005  37005        3   487     0
#> 3 0.143     1.630  1828    1828       Surry 37171  37171       86  3188     5
#> 4 0.070     2.968  1831    1831   Currituck 37053  37053       27   508     1
#> 5 0.153     2.206  1832    1832 Northampton 37131  37131       66  1421     9
#> 6 0.097     1.670  1833    1833    Hertford 37091  37091       46  1452     7
#>   NWBIR74 BIR79 SID79 NWBIR79                       geometry      value
#> 1      10  1364     0      19 MULTIPOLYGON (((-81.47276 3...  0.5497892
#> 2      10   542     3      12 MULTIPOLYGON (((-81.23989 3... -2.7449592
#> 3     208  3616     6     260 MULTIPOLYGON (((-80.45634 3...  0.5664575
#> 4     123   830     2     145 MULTIPOLYGON (((-76.00897 3...  0.4854790
#> 5    1066  1606     3    1197 MULTIPOLYGON (((-77.21767 3... -0.6263183
#> 6     954  1838     5    1237 MULTIPOLYGON (((-76.74506 3...  0.2547939
#>         sd  value_log10   sd_log2
#> 1 3.316465   1.43485757  3.195779
#> 2 1.056117   3.26255163 13.523692
#> 3 2.329557   8.42584616  1.962855
#> 4 3.337729   0.06256241  4.019426
#> 5 1.142165 105.56071379  4.708246
#> 6 2.064233   0.32936560  3.576428

plot(sf::st_geometry(nc))
```
