# Transform increments to thresholds

Transform increments to thresholds

## Usage

``` r
d_to_gamma(d)
```

## Arguments

- d:

  \[`numeric(J - 2)`\]  
  Threshold increments.

## Value

The threshold vector of length `J + 1`.

## Examples

``` r
d_to_gamma(c(0, 0, 0))
#>      [,1]
#> [1,] -Inf
#> [2,]    0
#> [3,]    1
#> [4,]    2
#> [5,]    3
#> [6,]  Inf
```
