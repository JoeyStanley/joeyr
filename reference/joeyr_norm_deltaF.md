# ∆F Normalization

Normalize vowel formant measurements with ∆F (see Johnson 2020). This
function is intended to be used within a tidyverse pipeline.

## Usage

``` r
joeyr_norm_deltaF(
  df,
  .F1,
  .F2,
  .F3,
  .F4,
  suffix = "_deltaF",
  return = "formants"
)

norm_deltaF(.df)
```

## Arguments

- df:

  The data frame containing the formant measurements you want to
  normalize

- .F1, .F2, .F3, .F4:

  The (unquoted) name of the column containing the F1 measurements. The
  first three are required, but you may leave off F4. It is recommended
  that you include F4 if the data is available and reliable since it
  produces more accurate results.

- suffix:

  A string. The suffix you'd like to append to column names in new
  normalized columns. By default, it's `"_deltaF"` so if your original
  F1 column was called `F1` then the normalized one will be `F1_deltaF`.

- return:

  A string. By default, it's `"formants"` so it'll return the normalized
  values for you. If you'd like to see the actual ΔF values, you can do
  so by putting `"deltaF"` instead.

## Value

The original dataframe with new columns containing the normalized
measurements.

## Details

The ∆F is a normalization technique that is based on a single,
interpretable parameter for each speaker. The parameter is called ∆F and
is "an estimate of formant spacing in a vocal tract with no
constrictions" (Johnson 2020:10).

You will need to group the data by speaker with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
before applying this function if you want to normalize the data by
speaker.

The data must be numeric, and there cannot be any `NA`s. So, if you're
using data extracted from Praat, you may have to filter out bad F3 and
F4 data and then convert the column to numeric.

Note that this is a new function and has not been tested very robustly
yet.

## Note

As of June 18, 2025, the \`norm_deltaF\` function is depreciated in
favor of the function of the same name in the tidynorm package. I
recommend you switch to that for future code. If you want to retain the
current functionality, you can call \`joeyr_norm_deltaF\` instead.

## References

Johnson, Keith. 2020. The ΔF Method of Vocal Tract Length Normalization
for Vowels. *Laboratory Phonology: Journal of the Association for
Laboratory Phonology* 11(1). <https://doi.org/10.5334/labphon.196>.

## Examples

``` r
library(tidyverse)
#> Error in library(tidyverse): there is no package called ‘tidyverse’
df <- joeysvowels::idahoans
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’

# Basic usage
df %>%
   group_by(speaker) %>%
   norm_deltaF(F1, F2, F3, F4)
#> Error in norm_deltaF(., F1, F2, F3, F4): unused arguments (F1, F2, F3, F4)

# F4 is not required
df %>%
   group_by(speaker) %>%
   norm_deltaF(F1, F2, F3)
#> Error in norm_deltaF(., F1, F2, F3): unused arguments (F1, F2, F3)

# Change the new columns' suffix
df %>%
   group_by(speaker) %>%
   norm_deltaF(F1, F2, F3, suffix = "_norm")
#> Error in norm_deltaF(., F1, F2, F3, suffix = "_norm"): unused arguments (F1, F2, F3, suffix = "_norm")

# Return ∆F instead
df %>%
   group_by(speaker) %>%
   norm_deltaF(F1, F2, F3, F4, return = "deltaF")
#> Error in norm_deltaF(., F1, F2, F3, F4, return = "deltaF"): unused arguments (F1, F2, F3, F4, return = "deltaF")
```
