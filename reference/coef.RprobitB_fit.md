# Extract posterior coefficient summaries

Returns posterior means or medians for population parameters or
individual random coefficients.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
coef(
  object,
  type = c("mean", "median"),
  level = c("population", "individual"),
  ...
)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- type:

  \[`character(1)`\]  
  The posterior summary to return:

  - `"mean"` averages the draws.

  - `"median"` is more robust for skewed posteriors.

- level:

  \[`character(1)`\]  
  Which parameters to return:

  - `"population"` returns the parameters shared by all deciders.

  - `"individual"` returns the random coefficients of every decider,
    which requires a mixed model fitted with
    `save_individual_draws = TRUE`.

- ...:

  Currently not used.

## Value

For `level = "population"`, a named numeric vector with one value per
global posterior variable that is not fixed by the normalization or the
model structure. For `level = "individual"`, a numeric matrix with
deciders in rows and random effects in columns. Log-normal coefficients
remain on their latent normal scale.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0,
  random_effects = "x",
  dgp_parameters = list(beta = c(x = 1), Omega = matrix(0.5)),
  chains = 1,
  save_individual_draws = TRUE
)
coef(model)
#>      mu[x] Omega[x,x] 
#>  1.1179023  0.4330456 
head(coef(model, level = "individual"))
#>          x
#> 1 1.225240
#> 2 1.093412
#> 3 1.234794
#> 4 1.152078
#> 5 1.205390
#> 6 1.255007
```
