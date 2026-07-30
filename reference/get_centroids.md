# Get centroids

Runs a summarizing function for each specified column, for each
specified group. This is intended to be used to plot centroids in
ellipses in ggplot2 without having to create a new object or have a lot
of in-line code. See examples below.

## Usage

``` r
get_centroids(df, .cols, ..., .fns = median)
```

## Arguments

- df:

  a dataframe.

- .cols:

  columns that should be summarized. For sociophonetic data, this is
  usually the names of your vowel columns, e.g. `c(F1, F2)`. This
  literally is just passed into an \`across\` function within
  \`summarize\`.

- ...:

  grouping variables. For sociophonetic data, this might be speaker and
  allophone or something. This is just passed into \`group_by\`.

- .fns:

  one or more names of functions. By default, `median`. This is passed
  into \`across\`.

## Value

an ungrouped dataframe

## Note

Okay technically this function name is a misnomer because we're not
truly getting centroids in a mathematical sense. But that's what I think
of when I run this so that's what we're going with.

## Examples

``` r
library(tidyverse)
#> Error in library(tidyverse): there is no package called ‘tidyverse’
df <- joeysvowels::idahoans
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’

# Basic usage as a summarizing function
df %>%
  get_centroids(c(F1, F2), vowel)
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"

# Within a ggplot2 block. Note that you do have to start the data argument with the dot and pipe it into get_centroids, rather than incorporating it in (i.e. get_centroids(., vowel)). Not sure why but this appears to be a contraint imposed by ggplot2.
ggplot(df, aes(F2, F1, color = vowel)) +
  stat_ellipse(level = 0.67) +
  geom_text(data = . %>% get_centroids(c(F1, F2), vowel), aes(label = vowel)) +
  scale_x_reverse() +
  scale_y_reverse() +
  theme(legend.position = "none")
#> Error in ggplot(df, aes(F2, F1, color = vowel)): `data` cannot be a function.
#> ℹ Have you misspelled the `data` argument in `ggplot()`?

# You can add multiple groups to the code too.
ggplot(df, aes(F2, F1, color = vowel)) +
  stat_ellipse(level = 0.67) +
  geom_text(data = . %>% get_centroids(c(F1, F2), speaker, vowel), aes(label = vowel)) +
  scale_x_reverse() +
  scale_y_reverse() +
  facet_wrap(~speaker, scales = "free") +
  theme(legend.position = "none")
#> Error in ggplot(df, aes(F2, F1, color = vowel)): `data` cannot be a function.
#> ℹ Have you misspelled the `data` argument in `ggplot()`?

# Like any use of group_by(), additional, perhaps redundant columns may be specified for the purpose of "passing them through." In this example, adding tense_lax doesn't change the calculations, but it's useful for this plot. Additionally, this block of code highlights one strength of get_centroids, and that is that I can pass in a modified dataframe directly to ggplot and then modify it even further to get the labels, without needing to create any new objects.
df %>%
  mutate(tense_lax = fct_collapse(vowel,
                                  "tense" = c("IY", "EY", "AO", "OW", "UW"),
                                  "lax"   = c("IH", "EH", "AE", "AA", "AH", "UH"))) %>%
  ggplot(aes(F2, F1, color = tense_lax, group = vowel)) +
  stat_ellipse(level = 0.67) +
  geom_text(data = . %>% get_centroids(c(F1, F2), speaker, tense_lax, vowel),
            aes(label = vowel)) +
  scale_x_reverse() +
  scale_y_reverse() +
  facet_wrap(~speaker, scales = "free") +
  theme(legend.position = "none")
#> Error in UseMethod("mutate"): no applicable method for 'mutate' applied to an object of class "function"

# For column selection, any tidyselect output works, such as matches().
df %>%
  get_centroids(matches("F\\d"), speaker, vowel)
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"

# For functions, you can add more than one. Just wrap them up into c().
df %>%
  get_centroids(c(F1, F2), .fns = c(median, mean), speaker, vowel)
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"

# However, unless they are named, they won't be useful.
df %>%
  get_centroids(c(F1, F2), .fns = c(`med` = median, `average` = mean), speaker, vowel)
#> Error in UseMethod("group_by"): no applicable method for 'group_by' applied to an object of class "function"
```
