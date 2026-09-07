# Preference-based classification of deciders

This function classifies the deciders based on their allocation to the
components of the mixing distribution.

## Usage

``` r
classification(x, add_true = FALSE)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

- add_true:

  Set to `TRUE` to add true class memberships to output (if available).

## Value

A `data.frame`.

The row names are the decider identifiers.

The first `C` columns contain the relative frequencies with which the
`deciders are allocated to classes. Next, the column`est\` contains the
estimated class of the decider based on the highest allocation
frequency.

If `add_true = TRUE`, the next column `true` contains the true class
memberships.

## Details

The relative frequencies of which each decider was allocated to the
classes during the Gibbs sampling are displayed. Only the thinned
samples after the burn-in period are considered.

## See also

[`update_z()`](https://loelschlaeger.de/RprobitB/reference/update_z.md)
for the updating function of the class allocation vector.
