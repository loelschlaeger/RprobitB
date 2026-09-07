# Extract model effects

This function extracts the estimated model effects.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
coef(object, ...)

# S3 method for class 'RprobitB_coef'
print(x, ...)

# S3 method for class 'RprobitB_coef'
plot(x, sd = 1, het = FALSE, ...)
```

## Arguments

- object:

  An object of class `RprobitB_fit`.

- ...:

  Currently not used.

- x:

  An object of class `RprobitB_coef`.

- sd:

  \[`integer(1)`\]  
  The number of standard deviations to display.

- het:

  \[`logical(1)`\]  
  Show the standard deviation of the mixing distribution? If `FALSE`,
  standard deviation of the estimate are shown.

## Value

An object of class `RprobitB_coef`.
