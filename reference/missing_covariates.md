# Handle missing covariates

This function checks for and replaces missing covariate entries in
`choice_data`.

## Usage

``` r
missing_covariates(
  choice_data,
  impute = "complete_cases",
  col_ignore = character()
)
```

## Arguments

- choice_data:

  \[`data.frame`\]  
  Choice data in wide format, where each row represents one choice
  occasion.

- impute:

  A character that specifies how to handle missing covariate entries in
  `choice_data`, one of:

  - `"complete_cases"`, removes all rows containing missing covariate
    entries (the default),

  - `"zero"`, replaces missing covariate entries by zero (only for
    numeric columns),

  - `"mean"`, imputes missing covariate entries by the mean (only for
    numeric columns).

- col_ignore:

  A character vector of columns that are ignored (none per default).

## Value

The input `choice_data`, in which missing covariates are addressed.
