# Update class sizes

Update class sizes

## Usage

``` r
update_m(C, z, non_zero = FALSE)
```

## Arguments

- C:

  \[`integer(1)`\]  
  The number (greater or equal 1) of latent classes of decision makers.

- z:

  \[`numeric(N)`\]  
  The decider class allocations.

- non_zero:

  \[`logical(1)`\]  
  Enforce strictly positive values in `m` (for numerical stability)?

## Value

An update for `m`.

## Examples

``` r
update_m(C = 4, z = c(1, 1, 1, 2, 2, 3))
#>      [,1]
#> [1,]    3
#> [2,]    2
#> [3,]    1
#> [4,]    0
```
