# Calculate Pillai scores

A function that runs a MANOVA and returns a summary statistics. For
`pillai` you get just the pillai score and for `manova_p` you get the
p-value. It works great within a tidyverse pipeline, though the function
itself uses only base R.

## Usage

``` r
pillai(...)

manova_p(...)
```

## Arguments

- ...:

  Arguments that may also be passed to
  [`manova()`](https://rdrr.io/r/stats/manova.html). Typically a formula
  and a dataframe.

## Value

The pillai score from the MANOVA test.

## Details

This function is best run when you've got your data properly subsetted
so that only two vowels' data are included. If you want to calculate the
pillai score for multiple speakers, you can do so by grouping the data
beforehand (using
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).

If you want to incorporate this function within a tidyverse pipeline,
it's best to do so within \`dplyr::summarize()\` since we are boiling
lot of data down to just one number. See the examples below.

Note that because it's just a MANOVA under the hood, you can incorporate
whatever variables you want within the model. However, currently, only
the pillai score of the first independent variable will be returned. So
if you want to include vowel and duration in the model, for example, but
you're mostly interested in the vowel, be sure to put it first in the
formula immediately after the tilde.

NAs won't crash the function, but observations with any NAs will be
excluded from analysis.

## Examples

``` r
suppressPackageStartupMessages(library(tidyverse))
options(dplyr.summarise.inform = FALSE)

# Look at the low back merger in one speaker.
one_speaker <- joeysvowels::midpoints
one_speaker %>%
  filter(vowel %in% c("LOT", "THOUGHT")) %>%
  summarize(pillai = pillai(cbind(F1, F2) ~ vowel))
#> # A tibble: 1 × 1
#>   pillai
#>    <dbl>
#> 1  0.444

# A non-tidyverse method
pillai(cbind(F1, F2) ~ vowel, data = subset(one_speaker, vowel %in% c("LOT", "THOUGHT")))
#> [1] 0.4437837

# Look at the low back merger in many speakers
many_speakers <- joeysvowels::idahoans
many_speakers %>%
  filter(vowel %in% c("AA", "AO")) %>%
  group_by(speaker) %>%
  summarize(pillai = pillai(cbind(F1, F2) ~ vowel))
#> # A tibble: 10 × 2
#>    speaker pillai
#>    <fct>    <dbl>
#>  1 01       0.592
#>  2 02       0.500
#>  3 03       0.763
#>  4 04       0.663
#>  5 05       0.557
#>  6 06       0.136
#>  7 07       0.412
#>  8 08       0.310
#>  9 09       0.173
#> 10 10       0.267

# Other variables can be included, but the pillai of only the first independent variable is returned
one_speaker %>%
  filter(vowel %in% c("LOT", "THOUGHT")) %>%
  mutate(dur = end - start) %>%
  summarize(pillai = pillai(cbind(F1, F2, F3, F4) ~ vowel + dur))
#> # A tibble: 1 × 1
#>   pillai
#>    <dbl>
#> 1  0.554

# Observations with NAs are excluded from the analysis
one_speaker[8,]$F1 <- NA
one_speaker %>%
  filter(vowel %in% c("LOT", "THOUGHT")) %>%
  summarize(pillai = pillai(cbind(F1, F2) ~ vowel))
#> # A tibble: 1 × 1
#>   pillai
#>    <dbl>
#> 1  0.441
```
