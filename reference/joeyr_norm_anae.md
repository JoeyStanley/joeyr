# ANAE Vowel Normalization

This is a a tidyverse-compatible function that makes it easy to
normalize your data using the method described in the Atlas of North
American English (Labov, Ash, & Boberg 2006).

## Usage

``` r
joeyr_norm_anae(df, hz_cols, token_id, speaker_id, g = "telsur")

norm_anae(.df)
```

## Arguments

- df:

  The dataframe containing the formant measurements you want to
  normalize.

- hz_cols:

  A list of columns (unquoted) containing the formant measurements
  themselves.

- token_id:

  The name of the column containing unique identifiers per vowel token.
  If your data is set up so that there is one row per token, you can put
  `row.names(.)` here instead.

- speaker_id:

  The name of the column containing unique identifiers per speaker
  (usually the column containing the speaker name).

- g:

  By default, `"telsur"`, whichwill use the Telsur G value (6.896874)
  listed in the ANAE. If set to `"calculate"`, it will calculate the G
  value based on the dataset. This can be set to any arbitrary number,
  such as `0` as well.

## Value

The same dataframe, but with new column(s), suffixed with "\_anae" that
have the normalized data.

## Details

The data must be grouped by speaker prior to running the function.

The function works best when only F1 and F2 data are included. F3 can be
included but the results may not be comparable with other studies.

By default, the function will use the Telsur G value listed in the ANAE
(6.896874) which will make the results most compatible with the ANAE and
other studies that use the same normalization procedure. The function
can calculate a G value based on the dataset provided when `g` is set to
`"calculate"`. Alternatively, `g` can be set to an arbitrary number,
such as zero.

It is unclear how the ANAE function should work with trajectory data.
This function pools all data together and normalizes it together, which
means one small modification was required to calculate the G value if
the Telsur G is not used: I had to add the average number of time points
per vowel token in the denominator. Not sure if that's how it should be
done, but it makes sense to me and returns sensible results.

## Note

As of June 18, 2025, the \`norm_anae\` function is depreciated in favor
of the various functions in the tidynorm package. I recommend you switch
to that for future code. If you want to retain the current
functionality, you can call \`joeyr_norm_anae\` instead.

## References

Labov, William, Sharon Ash, and Charles Boberg. *The Atlas of North
American English: Phonetics, Phonology and Sound Change.* Berlin: Walter
de Gruyter, 2006.

## Examples

``` r
library(tidyverse)
#> Error in library(tidyverse): there is no package called ‘tidyverse’
df <- joeysvowels::idahoans
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’

df %>%
   group_by(speaker) %>%
   norm_anae(hz_cols = c(F1, F2), speaker_id = speaker) %>%
   ungroup() %>%
   select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
#> Error in norm_anae(., hz_cols = c(F1, F2), speaker_id = speaker): unused arguments (hz_cols = c(F1, F2), speaker_id = speaker)

# Slightly different if G is calculated internally.
df %>%
   group_by(speaker) %>%
   norm_anae(hz_cols = c(F1, F2), speaker_id = speaker, g = "calculate") %>%
   ungroup() %>%
   select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
#> Error in norm_anae(., hz_cols = c(F1, F2), speaker_id = speaker, g = "calculate"): unused arguments (hz_cols = c(F1, F2), speaker_id = speaker, g = "calculate")

# G can be set to an arbitrary value.
df %>%
   group_by(speaker) %>%
   norm_anae(hz_cols = c(F1, F2), speaker_id = speaker, g = 0) %>%
   ungroup() %>%
   select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
#> Error in norm_anae(., hz_cols = c(F1, F2), speaker_id = speaker, g = 0): unused arguments (hz_cols = c(F1, F2), speaker_id = speaker, g = 0)
```
