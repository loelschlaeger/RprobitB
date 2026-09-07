# Create object of class `RprobitB_latent_classes`

This function creates an object of class `RprobitB_latent_classes` which
defines the number of latent classes and their updating scheme.

## Usage

``` r
RprobitB_latent_classes(latent_classes = NULL)

# S3 method for class 'RprobitB_latent_classes'
print(x, ...)
```

## Arguments

- latent_classes:

  \[[`list()`](https://rdrr.io/r/base/list.html) \| `NULL`\]  
  Optionally parameters specifying the number of latent classes and
  their updating scheme. The values in brackets are the default.

  - `C` (`1`): The fixed number (greater or equal 1) of (initial)
    classes.

  - `wb_update` (`FALSE`): Set to `TRUE` for weight-based class updates.

  - `dp_update` (`FALSE`): Set to `TRUE` for Dirichlet process class
    updates.

  - `Cmax` (`10`): The maximum number of latent classes.

  The following specifications are used for the weight-based updating
  scheme:

  - `buffer` (`50`): The number of iterations to wait before the next
    update.

  - `epsmin` (`0.01`): The threshold weight for removing a latent class.

  - `epsmax` (`0.7`): The threshold weight for splitting a latent class.

  - `deltamin` (`0.1`): The minimum mean distance before merging two
    classes.

  - `deltashift` (`0.5`): The scale for shifting the class means after a
    split.

- x:

  An object of class `RprobitB_latent_classes`.

- ...:

  Currently not used.

## Value

An object of class `RprobitB_latent_classes`.
