# Create lagged choice covariates

This function creates lagged choice covariates from the `data.frame`
`choice_data`, which is assumed to be sorted by the choice occasions.

## Usage

``` r
create_lagged_cov(choice_data, column = character(), k = 1, id = "id")
```

## Arguments

- choice_data:

  \[`data.frame`\]  
  Choice data in wide format, where each row represents one choice
  occasion.

- column:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Covariate names in `choice_data`.

- k:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The number of lags (in units of observations), see the details.

- id:

  \[`character(1)`\]  
  The name of the column in `choice_data` that contains unique
  identifier for each decision maker.

## Value

The input `choice_data` with the additional columns named `column.k` for
each element `column` and each number `k` containing the lagged
covariates.

## Details

Say that `choice_data` contains the column `column`. Then, the function
call


    create_lagged_cov(choice_data, column, k, id)

returns the input `choice_data` which includes a new column named
`column.k`. This column contains for each decider (based on `id`) and
each choice occasion the covariate faced before `k` choice occasions. If
this data point is not available, it is set to `NA`. In particular, the
first `k` values of `column.k` will be `NA` (initial condition problem).

## Examples

``` r
choice_data <- data.frame(id = rep(1:2, each = 3), cov = LETTERS[1:6])
create_lagged_cov(choice_data, column = "cov", k = 1:2)
#>   id cov cov.1 cov.2
#> 1  1   A  <NA>  <NA>
#> 2  1   B     A  <NA>
#> 3  1   C     B     A
#> 4  2   D  <NA>  <NA>
#> 5  2   E     D  <NA>
#> 6  2   F     E     D
```
