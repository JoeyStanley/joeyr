# Low-Back-Merger Shift Index

A function to quickly calculate the Low-Back-Merger Shift Index, per
Becker (2019).

## Usage

``` r
lbms_index(df, vowel_col, F1_col, F2_col, beet, bit, bet, bat)
```

## Arguments

- df:

  The dataframe containing the formant measurements you want to base the
  calculation off of.

- vowel_col:

  The name of the column containing the name of the vowel (e.g. `vowel`)

- F1_col:

  The name of the column containing the F1 measurements (e.g. `F1_norm`)

- F2_col:

  The name of the column containing the F2 measurements (e.g. `F2_norm`)

- beet:

  A string that indicates which vowels belong to the BEET class of words
  (e.g. `"IY"`)

- bit:

  A string that indicates which vowels belong to the BIT class of words
  (e.g. `"IH"`)

- bet:

  A string that indicates which vowels belong to the BET class of words
  (e.g. `"EH"`)

- bat:

  A string that indicates which vowels belong to the BAT class of words
  (e.g. `"AE"`)

## Value

A `summarize`d dataframe with the LBMS Index.

## Details

If you would like to calculate the LBMS Index for each speaker, be sure
to the data beforehand with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) (see
the examples). Also, per Becker's (2019) recommendation, it is
recommended that you exclude tokens before nasals, laterals, rhotics,
and /g/ before the calculation. It is also recommended to run the
function on Lobanov-normalized data.

Note that this is a new function and has not been tested robustly yet.

## References

Becker, Kara, ed. The Low-Back-Merger Shift: Uniting the Canadian Vowel
Shift, the California Vowel Shift, and Short Front Vowel Shifts across
North America. Publication of the American Dialect Society 104. Durham,
NC: Duke University Press, 2019.

## Examples

``` r
suppressPackageStartupMessages(library(tidyverse))
#> Error in library(tidyverse): there is no package called ‘tidyverse’
# This dataset has following segment and normalized data already
darla <- joeysvowels::darla
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’
darla %>%
  filter(!fol_seg %in% c("L", "R", "M", "N", "NG", "G")) %>%
  lbms_index(vowel, F1_LobanovNormed_unscaled, F2_LobanovNormed_unscaled, "IY", "IH", "EH", "AE")
#> Error in pivot_wider(., names_from = .vowel, values_from = c(.F1, .F2)): could not find function "pivot_wider"

# This dataset has multiple speakers and needs to be normalized
idahoans <- joeysvowels::idahoans
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’
idahoans %>%
  group_by(speaker) %>%
  mutate(F1_lob = scale(F1),
         F2_lob = scale(F2)) %>%
  lbms_index(vowel, F1_lob, F2_lob, "IY", "IH", "EH", "AE")
#> Error in pivot_wider(., names_from = .vowel, values_from = c(.F1, .F2)): could not find function "pivot_wider"
```
