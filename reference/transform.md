# Transform fitted probit model

Given an object of class `RprobitB_fit`, this function can:

- change the length `B` of the burn-in period,

- change the the thinning factor `Q` of the Gibbs samples,

- change the utility `scale`.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
transform(
  `_data`,
  B = NULL,
  Q = NULL,
  scale = NULL,
  check_preference_flip = TRUE,
  ...
)
```

## Arguments

- \_data:

  An object of class
  [`RprobitB_fit`](https://loelschlaeger.de/RprobitB/reference/RprobitB_fit.md).

- B:

  \[`integer(1)`\]  
  The length of the burn-in period.

- Q:

  \[`integer(1)`\]  
  The thinning factor for the Gibbs samples.

- scale:

  \[`character(1)`\]  
  A character which determines the utility scale. It is of the form
  `<parameter> := <value>`, where `<parameter>` is either the name of a
  fixed effect or `Sigma_<j>,<j>` for the `<j>`th diagonal element of
  `Sigma`, and `<value>` is the value of the fixed parameter.

- check_preference_flip:

  Set to `TRUE` to check for flip in preferences after new `scale`.

- ...:

  Currently not used.

## Value

The transformed `RprobitB_fit` object.

## Details

See the vignette "Model fitting" for more details:
[`vignette("v03_model_fitting", package = "RprobitB")`](https://loelschlaeger.de/RprobitB/articles/v03_model_fitting.md).
