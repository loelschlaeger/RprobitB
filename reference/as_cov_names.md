# Re-label alternative specific covariates

In `{RprobitB}`, alternative specific covariates must be named in the
format `"<covariate>_<alternative>"`. This helper function generates the
format for a given `choice_data` set.

## Usage

``` r
as_cov_names(choice_data, cov, alternatives)
```

## Arguments

- choice_data:

  \[`data.frame`\]  
  Choice data in wide format, where each row represents one choice
  occasion.

- cov:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Names of alternative specific covariates in `choice_data`.

- alternatives:

  \[`atomic()`\]  
  The alternative names.

## Value

The `choice_data` input with updated column names.

## Examples

``` r
data("Electricity", package = "mlogit")
cov <- c("pf", "cl", "loc", "wk", "tod", "seas")
alternatives <- 1:4
colnames(Electricity)
#>  [1] "choice" "id"     "pf1"    "pf2"    "pf3"    "pf4"    "cl1"    "cl2"   
#>  [9] "cl3"    "cl4"    "loc1"   "loc2"   "loc3"   "loc4"   "wk1"    "wk2"   
#> [17] "wk3"    "wk4"    "tod1"   "tod2"   "tod3"   "tod4"   "seas1"  "seas2" 
#> [25] "seas3"  "seas4" 
Electricity <- as_cov_names(Electricity, cov, alternatives)
colnames(Electricity)
#>  [1] "choice" "id"     "pf_1"   "pf_2"   "pf_3"   "pf_4"   "cl_1"   "cl_2"  
#>  [9] "cl_3"   "cl_4"   "loc_1"  "loc_2"  "loc_3"  "loc_4"  "wk_1"   "wk_2"  
#> [17] "wk_3"   "wk_4"   "tod_1"  "tod_2"  "tod_3"  "tod_4"  "seas_1" "seas_2"
#> [25] "seas_3" "seas_4"
```
