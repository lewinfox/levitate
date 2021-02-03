
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Fuzzy string matching for R

<!-- badges: start -->

<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/levitate)](https://cran.r-project.org/package=levitate) -->

[![Travis build
status](https://travis-ci.com/lewinfox/levitate.svg?branch=main)](https://travis-ci.com/lewinfox/levitate)
<!-- badges: end -->

`levitate` is based on the Python
[fuzzywuzzy](https://github.com/seatgeek/fuzzywuzzy) package for fuzzy
string matching. An R port of this already exists, but unlike
[fuzzywuzzyR](https://github.com/mlampros/fuzzywuzzyR), `levitate` is
written entirely in R with no external dependencies on `reticulate` or
Python. It also offers a couple of extra bells and whistles in the form
of vectorised functions.

## Why “`levitate`”?

A common measure of string similarity is the [**Lev**enshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance), and the
name was available on CRAN.

## Examples

### `lev_distance()`

The edit distance is the number of additions, subtractions or
substitutions needed to transform one string into another. Base R
provides the `adist()` function to compute this. `levitate` provides
`lev_distance()` which is powered by the
[`stringdist`](https://github.com/markvanderloo/stringdist) package.

``` r
lev_distance("cat", "bat")
#> [1] 1

lev_distance("rat", "rats")
#> [1] 1

lev_distance("cat", "rats")
#> [1] 2
```

The function can accept vectorised input. Where the inputs have a
`length()` greater than 1 the results are returned as a matrix.

``` r
lev_distance(c("cat", "dog", "clog"), c("rat", "log", "frog"))
#>      rat log frog
#> cat    1   3    4
#> dog    3   1    2
#> clog   4   1    2
```

However if at least one (or both) of the inputs is scalar (length 1) we
can return a vector. The elements of the vector are named based on the
longer input.

``` r
lev_distance(c("cat", "dog", "clog"), "rat")
#>  cat  dog clog 
#>    1    3    4

lev_distance("cat", c("rat", "log", "frog", "other"))
#>   rat   log  frog other 
#>     1     3     4     5
```

### `lev_ratio()`

More useful than the edit distance, `lev_ratio()` makes it easier to
compare similarity across different strings. Identical strings will get
a score of 1 and entirely dissimilar strings will get a score of 0.

This function behaves exactly like `lev_distance()`:

``` r
lev_ratio("cat", "bat")
#> [1] 0.6666667

lev_ratio("rat", "rats")
#> [1] 0.75

lev_ratio("cat", "rats")
#> [1] 0.5

lev_ratio(c("cat", "dog", "clog"), c("rat", "log", "frog"))
#>            rat       log      frog
#> cat  0.6666667 0.0000000 0.0000000
#> dog  0.0000000 0.6666667 0.3333333
#> clog 0.0000000 0.7500000 0.5000000
```

### `lev_partial_ratio()`

*TODO*

### `lev_token_sort_ratio()`

*TODO*

### `lev_token_set_ratio()`

*TODO*

## Porting code from `fuzzywuzzy` or `fuzzywuzzyR`

The underlying algorithms differ between `levitate` and `fuzzywuzzy`,
not least because
[`stringdist`](\(https://github.com/markvanderloo/stringdist\)) offers
several possible similarity measures. Be careful if you are porting code
that relies on hard-coded or learned cutoffs for similarity measures.
