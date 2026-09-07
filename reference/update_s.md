# Update class weight vector

Update class weight vector

## Usage

``` r
update_s(delta, m)
```

## Arguments

- delta:

  \[`numeric(1)`\]  
  The prior concentration for `s`.

- m:

  \[`numeric(C)`\]  
  The vector of current class frequencies.

## Value

An update for `s`.

## Examples

``` r
update_s(delta = 1, m = 4:1)
#>            [,1]
#> [1,] 0.30358427
#> [2,] 0.29550515
#> [3,] 0.08132054
#> [4,] 0.31959004
```
