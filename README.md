
<!-- README.md is generated from README.Rmd. Please edit that file -->

# levitate <img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![](http://cranlogs.r-pkg.org/badges/grand-total/levitate)](https://cran.r-project.org/package=levitate)
[![R-CMD-check](https://github.com/lewinfox/levitate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lewinfox/levitate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`levitate` is based on the Python
[thefuzz](https://github.com/seatgeek/thefuzz) (formerly `fuzzywuzzy`)
package for fuzzy string matching. An R port of this already exists, but
unlike [fuzzywuzzyR](https://github.com/mlampros/fuzzywuzzyR),
`levitate` is written entirely in R with no external dependencies on
`reticulate` or Python. It also offers a couple of extra bells and
whistles in the form of vectorised functions.

View the docs at <https://lewinfox.com/levitate/>.

## Why “`levitate`”?

A common measure of string similarity is the [**Lev**enshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance), and the
name was available on CRAN.

**NOTE** The default distance metric is Optimal String Alignment (OSA),
not Levenshtein distance. This is the default method used by the
`stringdist` package, which `levitate` uses for distance calculations.
OSA allows transpositions whereas Levenshtein distance does not. To use
Levenshtein distance pass `method = "lv"` to any `lev_*()` functions.

``` r
lev_distance("01", "10") # Transpositions allowed by the default `method = "osa"`
#> [1] 1

lev_distance("01", "10", method = "lv") # No transpositions
#> [1] 2
```

A full list of distance metrics is available in
`help("stringdist-metrics", package = stringdist)`.

## Installation

Install the released version from CRAN:

``` r
install.packages("levitate")
```

Alternatively, you can install the development version from Github:

``` r
devtools::install_github("lewinfox/levitate")
```

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
`length()` greater than 1 the results are returned as a vector unless
`pairwise = FALSE`, in which case a matrix is returned.

``` r
lev_distance(c("cat", "dog", "clog"), c("rat", "log", "frog"))
#> [1] 1 1 2

lev_distance(c("cat", "dog", "clog"), c("rat", "log", "frog"), pairwise = FALSE)
#>      rat log frog
#> cat    1   3    4
#> dog    3   1    2
#> clog   4   1    2
```

If at least one (or both) of the inputs is scalar (length 1) the result
will be a vector. The elements of the vector are named based on the
longer input (unless `useNames = FALSE`).

``` r
lev_distance(c("cat", "dog", "clog"), "rat")
#>  cat  dog clog 
#>    1    3    4

lev_distance("cat", c("rat", "log", "frog", "other"))
#>   rat   log  frog other 
#>     1     3     4     5

lev_distance("cat", c("rat", "log", "frog", "other"), useNames = FALSE)
#> [1] 1 3 4 5
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
#> [1] 0.6666667 0.6666667 0.5000000
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
#> [1] 0.9354839
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

### `lev_weighted_token_ratio()`

The `lev_weighted_*()` family of functions work slightly differently
from the others. They always tokenise their input, and they allow you to
assign different weights to specific tokens. This allows you to exert
some influence over parts of the input strings that are more or less
interesting to you.

For example, maybe you’re comparing company names from different
sources, trying to match them up.

``` r
lev_ratio("united widgets, ltd", "utd widgets, ltd") # Note the typos
#> [1] 0.8421053
```

These strings score quite highly already, but the `"ltd"` in each name
isn’t very helpful. We can use `lev_weighted_token_ratio()` to reduce
the impact of `"ltd"`.

**NOTE** Because the tokenisation affects the score, we can’t compare
the output of the `lev_weighted_*()` functions with the non-weighted
versions. To get a baseline, call the weighted function without
supplying a `weights` argument.

``` r
lev_weighted_token_ratio("united widgets, ltd", "utd widgets, ltd")
#> [1] 0.8125

lev_weighted_token_ratio("united widgets, ltd", "utd widgets, ltd", weights = list(ltd = 0.1))
#> [1] 0.7744361
```

De-weighting `"ltd"` has reduced the similarity score of the strings,
which gives a more accurate impression of their similarity.

We can remove the effect of `"ltd"` altogether by setting its weight to
zero.

``` r
lev_weighted_token_ratio("united widgets, ltd", "utd widgets, ltd", weights = list(ltd = 0))
#> [1] 0.7692308

lev_weighted_token_ratio("united widgets", "utd widgets")
#> [1] 0.7692308
```

De-weighting also works the other way - if the token to be weighted
appears in one string but not the other, then de-weighting it
*increases* the similarity score:

``` r
lev_weighted_token_ratio("utd widgets", "united widgets, ltd")
#> [1] 0.625

lev_weighted_token_ratio("utd widgets", "united widgets, ltd", weights = list(ltd = 0.1))
#> [1] 0.7518797
```

#### Limitations of token weighting

`lev_weighted_token_ratio()` has a key limitation: tokens will only be
weighted if:

- The token appears in the same position in both strings (i.e. it’s the
  first/second/third, etc. token in both)
- OR the strings contain different numbers of tokens, and the
  corresponding token position in the other string is empty.

This is probably easiest to see by example.

``` r
lev_weighted_token_ratio("utd widgets limited", "united widgets, ltd")
#> [1] 0.65
lev_weighted_token_ratio("utd widgets limited", "united widgets, ltd", weights = list(ltd = 0.1, limited = 0.1))
#> [1] 0.65
```

In this case the weighting has had no effect. Why not? Internally, the
function has tokenised the strings as follows:

| token_1  | token_2   | token_3   |
|----------|-----------|-----------|
| “utd”    | “widgets” | “limited” |
| “united” | “widgets” | “ltd”     |

Because the token `"ltd"` doesn’t appear in the same position in both
strings, the function doesn’t apply any weights.

This is a deliberate decision; while in the example above it’s easy to
say “well, clearly ltd and limited are the same thing so we ought to
weight them”, how should we handle a less clear example?

``` r
lev_weighted_token_ratio("green eggs and ham", "spam spam spam spam")
#> [1] 0.1176471
lev_weighted_token_ratio("green eggs and ham", "spam spam spam spam", weights = list(spam = 0.1, eggs = 0.5))
#> [1] 0.1176471
```

In this case it’s hard to say what the “correct” approach would be.
There isn’t a meaningful way of applying weights to dissimilar tokens.
In situations like “ltd”/“limited”, a pre-cleaning or standardisation
process might be helpful, but that is outside the scope of what
`levitate` offers.

I recommend exploring `lev_weighted_token_sort_ratio()` and
`lev_weighted_token_set_ratio()` as they may give more useful results
for some problems. Remember, **weighting is going to be most useful when
compared to the unweighted output of the same function**.

## Ranking functions

A common problem in this area is “given a string x and a set of strings
y, which string in y is most / least similar to x?”. `levitate` provides
two functions to help with this: `lev_score_multiple()` and
`lev_best_match()`.

`lev_score_multiple()` returns a ranked list of candidates. By default
the highest-scoring is first.

``` r
lev_score_multiple("bilbo", c("gandalf", "frodo", "legolas"))
#> $frodo
#> [1] 0.2
#> 
#> $legolas
#> [1] 0.1428571
#> 
#> $gandalf
#> [1] 0
```

`lev_best_match()` returns the best matched string without any score
information.

``` r
lev_best_match("bilbo", c("gandalf", "frodo", "legolas"))
#> [1] "frodo"
```

Both functions take a `.fn` argument which allows you to select a
different ranking function. The default is `lev_ratio()` but you can
pick another or write your own. See `?lev_score_multiple` for details.

You can also reverse the direction of sorting by using
`decreasing = FALSE`. This reverses the sort direction so *lower*
scoring items are preferred. This may be helpful if you’re using a
distance rather than a similarity measure, or if you want to return
least similar strings.

``` r
lev_score_multiple("bilbo", c("gandalf", "frodo", "legolas"), decreasing = FALSE)
#> $gandalf
#> [1] 0
#> 
#> $legolas
#> [1] 0.1428571
#> 
#> $frodo
#> [1] 0.2
```

## Porting code from `thefuzz` or `fuzzywuzzyR`

Results differ between `levitate` and `thefuzz`, not least because
[`stringdist`](https://github.com/markvanderloo/stringdist) offers
several possible similarity measures. Be careful if you are porting code
that relies on hard-coded or learned cutoffs for similarity measures.
