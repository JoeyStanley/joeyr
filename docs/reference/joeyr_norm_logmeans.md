# Log-means normalization

Normalize vowel formant measurements a log-means normalization procedure
as described in Barreda & Nearey (2018). This function is intended to be
used within a tidyverse pipeline.

## Usage

``` r
joeyr_norm_logmeans(
  .df,
  .formant_cols,
  .speaker_col,
  .vowel_col,
  .return = "data",
  i_know_more_than_you = FALSE
)

norm_logmeans(.df)
```

## Arguments

- .df:

  The data frame containing the formant measurements you want to
  normalize. **Formant data must be log transformed!** See example code
  below.

- .formant_cols:

  The (unquoted) name(s) of the column containing the formant
  measurements.

- .speaker_col:

  The (unquoted) name of the column containing the unique identifiers
  per speaker.

- .vowel_col:

  The (unquoted) name of the column containing the unique identifiers
  per vowel

- .return:

  A string. By default, `"data"`, which will returned the your original
  data with the normalized data appended. If you set this to `"params"`,
  you'll get a data frame with the normalization paramters for each
  speakers.

- i_know_more_than_you:

  Logical. The function won't work if you've got data that doesn't look
  like log-transformed formant data. If you want to force the function
  to run anyway, set this to \`TRUE\`.

## Value

The original dataframe with new columns containing the normalized
measurements. These new columns have "\_norm" appended to the column
names.

## Details

The data should *not* be grouped beforehand (e.g. with `group_by`). The
data must be numeric, and there cannot be any `NA`s.

## Note

As of June 18, 2025, the \`norm_logmeans\` function is depreciated in
favor of the \`norm_nearey\` function in the tidynorm package. I
recommend you switch to that for future code. If you want to retain the
current functionality, you can call \`joeyr_norm_logmeans\` instead.

Thanks to Santiago Barreda for providing most of the code for this
function.

## References

Barreda, Santiago, and Terrance M. Nearey. 2018. "A Regression Approach
to Vowel Normalization for Missing and Unbalanced Data." *The Journal of
the Acoustical Society of America* 144(1): 500–520.
<https://doi.org/10.1121/1.5047742>.

## Examples

``` r
library(tidyverse)
idaho <- joeysvowels::idahoans

# Basic usage. Note that the data has to be log10-transformed.
idaho %>%
    mutate(F1_log = log10(F1), F2_log = log10(F2)) %>%
    norm_logmeans(.formant_cols = c(F1_log, F2_log),
                  .speaker_col = speaker,
                  .vowel_col = vowel) %>%
    head()
#> Error in norm_logmeans(., .formant_cols = c(F1_log, F2_log), .speaker_col = speaker,     .vowel_col = vowel): unused arguments (.formant_cols = c(F1_log, F2_log), .speaker_col = speaker, .vowel_col = vowel)

# Return the speaker paramters instead.
idaho %>%
    mutate(F1_log = log10(F1), F2_log = log10(F2)) %>%
    norm_logmeans(.formant_cols = c(F1_log, F2_log),
                  .speaker_col = speaker,
                  .vowel_col = vowel,
                  .return = "params") %>%
    head()
#> Error in norm_logmeans(., .formant_cols = c(F1_log, F2_log), .speaker_col = speaker,     .vowel_col = vowel, .return = "params"): unused arguments (.formant_cols = c(F1_log, F2_log), .speaker_col = speaker, .vowel_col = vowel, .return = "params")

# If you forget to log-transform the data, it'll throw an error.
idaho %>%
    norm_logmeans(.formant_cols = c(F1, F2),
                  .speaker_col = speaker,
                  .vowel_col = vowel)
#> Error in norm_logmeans(., .formant_cols = c(F1, F2), .speaker_col = speaker,     .vowel_col = vowel): unused arguments (.formant_cols = c(F1, F2), .speaker_col = speaker, .vowel_col = vowel)

# But you can force the function to run on non-transformed data if you're sure you know what you're doing.
idaho %>%
    norm_logmeans(.formant_cols = c(F1, F2),
                  .speaker_col = speaker,
                  .vowel_col = vowel,
                  i_know_more_than_you = TRUE) %>%
    head()
#> Error in norm_logmeans(., .formant_cols = c(F1, F2), .speaker_col = speaker,     .vowel_col = vowel, i_know_more_than_you = TRUE): unused arguments (.formant_cols = c(F1, F2), .speaker_col = speaker, .vowel_col = vowel, i_know_more_than_you = TRUE)
```
