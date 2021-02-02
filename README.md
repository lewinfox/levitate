
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Fuzzy string matching for R

`levitate` is based on the Python
[fuzzywuzzy](https://github.com/seatgeek/fuzzywuzzy) package for fuzzy
string matching. An R port of this already exists, but unlike
[fuzzywuzzyR](https://github.com/mlampros/fuzzywuzzyR), `levitate` is
written entirely in R with no external dependencies on `reticulate` or
Python. It also offers a couple of extra bells and whistles in the form
of vectorised functions.

## Why “`levitate`”?

The workhorse of the package is the [**Lev**enshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance), and the
name was available on CRAN.

## Usage

### Edit distance

The edit distance is the number of additions, subtractions or
substitutions needed to transform one string into another. Base R
provides the `adist()` function to compute this. `levitate` provides
`lev_distance()`.

``` r
lev_distance("cat", "bat")
#> [1] 1

lev_distance("rat", "rats")
#> [1] 1

lev_distance("cat", "rats")
#> [1] 2
```

#### Vectorisation

The function can accept vectorised input. Where the inputs have a
`length()` greater than 1 the results are returned as a matrix.

``` r
lev_distance(c("cat", "dog", "clog"), c("rat", "log", "frog"))
#>      rat log frog
#> cat    1   3    4
#> dog    3   1    2
#> clog   4   1    2
```

#### Pairwise comparison

Sometimes you might not be interested in an all-to-all comparison. All
`lev_*()` functions offer a `pairwise` option which compares `a[1]`
against `b[1]`, `a[2]` against `b[2]`,
etc.

``` r
lev_distance(c("cat", "dog", "clog"), c("rat", "log", "frog"), pairwise = TRUE)
#> [1] 1 1 2
```

`pairwise = TRUE` works by taking the `diag()` (onal) of the matrix.
This is only possible if the matrix has an equal number of rows and
columns, i.e. if the inputs are the same
length.

``` r
lev_distance(c("cat", "dog", "clog"), c("rat", "log", "frog", "other"), pairwise = TRUE)
#> x Result has 3 rows and 4 columns. For a pairwise result the number of rows and columns must match, or one or both must be 1.
#> Error: Cannot compute pairwise result
```

However if at least one (or both) of the inputs is scalar (length 1) we
can return a vector. The elements of the vector are named based on the
longer input.

``` r
lev_distance(c("cat", "dog", "clog"), "rat", pairwise = TRUE)
#>  cat  dog clog 
#>    1    3    4

lev_distance("cat", c("rat", "log", "frog", "other"), pairwise = TRUE)
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
#> [1] 0.8333333

lev_ratio("rat", "rats")
#> [1] 0.8571429

lev_ratio("cat", "rats")
#> [1] 0.7142857

lev_ratio(c("cat", "dog", "clog"), c("rat", "log", "frog"))
#>            rat       log      frog
#> cat  0.8333333 0.0000000 0.0000000
#> dog  0.0000000 0.8333333 0.7142857
#> clog 0.0000000 0.8571429 0.7500000

lev_ratio(c("cat", "dog", "clog"), c("rat", "log", "frog"), pairwise = TRUE)
#> [1] 0.8333333 0.8333333 0.7500000
```

## Comparison to Python `fuzzywuzzy` or `fuzzywuzzyR`

The Python implementation has a different algorithm for calculating
similarity to `levitate` meaning that the packages give different
results where the input strings are the same length:

``` python
>>> fuzz.ratio("cat", "bat")
67
```

``` r
lev_ratio("cat", "bat")
#> [1] 0.8333333
```

This is due to the fact that `fuzzywuzzy` calculates the number of
*matches* between the strings whereas `levitate` calculates the
*differences*. In the “cat” / “bat” example above, Python is calculating
two matches between the strings and returning `2 * n_matches /
(len("cat") + len("bat"))`, i.e. `4 / 6` = 0.667 (67% after rounding).
On the other hand `levitate` is finding a string distance of 1 and
returning `(nchar("cat") + nchar("bat") - 1) / (nchar("cat") +
nchar("bat"))` i.e. `5 / 6` = 0.83.

The Python approach looks nicer for short input where you could work it
out yourself (e.g. most people will probably agree that “cat” and “bat”
are 66% the same, not 85%), but `levitate`’s algorithm takes advantage
of the built-in `adist()` function and avoids having to implement a
token matcher. As the input gets longerthe scores converge, and the
relative ranking of candidate strings is preserved between the two
implementations.

``` python
>>> fuzz.ratio("supercalifragilisticexpialidocious", "supercalifragilisticexpialidocius")
99
>>> fuzz.ratio("supercalifragilisticexpialidocious", "supercalifragilisticexpialidociuss")
97
```

``` r
lev_ratio("supercalifragilisticexpialidocious", "supercalifragilisticexpialidocius")
#> [1] 0.9850746

lev_ratio("supercalifragilisticexpialidocious", "supercalifragilisticexpialidociuss")
#> [1] 0.9705882
```

We can see that `levitate` is a little less generous with its scoring.
This will be relevant if you are porting existing Python code to R and
you’ve got some logic that uses hard-coded similarity thresholds.
