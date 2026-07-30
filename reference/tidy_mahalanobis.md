# Calculate Mahalanobis Distance

This is a tidyverse-tidyverse-compatible version of the
[`stats::mahalanobis`](https://rdrr.io/r/stats/mahalanobis.html)
function. It just makes it easier to include it as part of a
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
tidy_mahalanobis(...)
```

## Arguments

- ...:

  Names of columns that should be included in the Mahalanobis distance.
  For vowel data, this is typically your F1 and F2 columns.

## Value

A vector that contains the Mahalanobis distances for each observation.

## Details

Typically you'll want to group your data (using
[`dplyr::group_by`](https://dplyr.tidyverse.org/reference/group_by.html))
by speaker and vowel class so that you get the distance from vowel
centroids.

I won't tell you what to do with those distances, but if you might
consider looking at tokens where the square root of the Mahalanobis
distance is greater than around 2. However, to be clear, the exact
cutoff will vary depending on the size and variability of your data. You
can see how you might isolate these points visually in the example code
below.

One small modification that this function does that
[`stats::mahalanobis`](https://rdrr.io/r/stats/mahalanobis.html) does
not do is that if there are fewer than 5 measurements in a group,
`tidy_mahalanobis` returns them all as having a distance of zero. I
found that this prevents some fatal errors from crashing the script when
running this function on smaller datasets.

Note that this function requires the `MASS` package to be installed to
work, but you don't need to load it.

## Examples

``` r
suppressPackageStartupMessages(library(tidyverse))
#> Error in library(tidyverse): there is no package called ‘tidyverse’
df <- joeysvowels::midpoints
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’

# Calculate the distances
m_dists <- df %>%
  group_by(vowel) %>%
  mutate(mahal_dist = tidy_mahalanobis(F1, F2))
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"

# Take a peek at the resulting dataset
m_dists %>%
  select(vowel, F1, F2, mahal_dist) %>%
  head()
#> Error: object 'm_dists' not found

# Plot potential outliers
ggplot(m_dists, aes(F2, F1, color = sqrt(mahal_dist) > 2)) +
   geom_point() +
   scale_x_reverse() +
   scale_y_reverse()
#> Error: object 'm_dists' not found

# You can include whatever numeric variables you want, like duration
df %>%
  group_by(vowel) %>%
  mutate(dur = end - start) %>%
  mutate(mahal_dist = tidy_mahalanobis(F1, F2, dur)) %>%
  ggplot(aes(F2, F1, color = sqrt(mahal_dist) > 2.5)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"

# Data cannot contain NAs. Remove them before running.
df[1,]$F1 <- NA
#> Error in `*tmp*`[1, ]: object of type 'closure' is not subsettable
df %>%
  group_by(vowel) %>%
  mutate(mahal_dist = tidy_mahalanobis(F1, F2))
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"
df %>%
  group_by(vowel) %>%
  filter(!is.na(F1)) %>%
  mutate(mahal_dist = tidy_mahalanobis(F1, F2)) %>%
  select(vowel_id, vowel, mahal_dist, F1, F2) %>%
  head()
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"
```
