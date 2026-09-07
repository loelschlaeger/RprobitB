# Extract covariates of choice occasion

This helper function returns the covariates and the choices of specific
choice occasions.

## Usage

``` r
get_cov(x, id = NULL, idc = NULL, id_label = NULL, idc_label = NULL)
```

## Arguments

- x:

  Either an object of class `RprobitB_data` or `RprobitB_fit`.

- id, idc:

  \[[`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`\]  
  Identifiers for deciders and choice occasions.

  If `NULL`, everything is returned.

- id_label, idc_label:

  \[`character(1)` \| `NULL`\]  
  The columns that contain the decider and choice occasion identifier.

  If `NULL`, this information is extracted from `x`.

## Value

A subset of the `choice_data` data frame specified in
[`prepare_data()`](https://loelschlaeger.de/RprobitB/reference/prepare_data.md).

## Examples

``` r
data <- simulate_choices(
  form = product ~ price,
  N = 10,
  T = 1:10,
  J = 3,
  ranked = TRUE
)
get_cov(data, id = 2)
#>   id idc    price_A   price_B    price_C product
#> 2  2   1  3.4973930 0.1723737  1.0769661   B,C,A
#> 3  2   2 -0.7832629 1.4813826 -0.1431028   A,C,B
```
