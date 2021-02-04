
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

View the docs at <https://lewinfox.github.io/levitate/>.

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

If `a` and `b` are different lengths, this function compares all the
substrings of the longer string that are the same length as the shorter
string and returns the highest `lev_ratio()` of all of them. E.g. when
comparing `"actor"` and `"tractor"` we would compare `"actor"` with
`"tract"`, `"racto"` and `"actor"` and return the highest score (in this
case 1).

``` r
lev_partial_ratio("actor", "tractor")
#> [1] 1

# What's actually happening is the max() of this result is being returned
lev_ratio("actor", c("tract", "racto", "actor"))
#> tract racto actor 
#>   0.2   0.6   1.0
```

### `lev_token_sort_ratio()`

The inputs are tokenised and the tokens are sorted alphabetically, then
the resulting strings are compared.

``` r
x <- "Episode IV - Star Wars: A New Hope"
y <- "Star Wars Episode IV - New Hope"

# Because the order of words is different the simple approach gives a low match ratio.
lev_ratio(x, y)
#> [1] 0.3529412

# The sorted token approach ignores word order.
lev_token_sort_ratio(x, y)
#> [1] 0.9117647
```

### `lev_token_set_ratio()`

Similar to `lev_token_sort_ratio()` this function breaks the input down
into tokens. It then identifies any common tokens between strings and
creates three new strings:

    x <- {common_tokens}
    y <- {common_tokens}{remaining_unique_tokens_from_string_a}
    z <- {common_tokens}{remaining_unique_tokens_from_string_b}

and performs three pairwise `lev_ratio()` calculations between them (`x`
vs `y`, `y` vs `z` and `x` vs `z`). The highest of those three ratios is
returned.

``` r
x <- "the quick brown fox jumps over the lazy dog"
y <- "my lazy dog was jumped over by a quick brown fox"

lev_ratio(x, y)
#> [1] 0.2916667

lev_token_sort_ratio(x, y)
#> [1] 0.6458333

lev_token_set_ratio(x, y)
#> [1] 0.7435897
```

## Porting code from `fuzzywuzzy` or `fuzzywuzzyR`

Results differ between `levitate` and `fuzzywuzzy`, not least because
[`stringdist`](https://github.com/markvanderloo/stringdist) offers
several possible similarity measures. Be careful if you are porting code
that relies on hard-coded or learned cutoffs for similarity measures.
