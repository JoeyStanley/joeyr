# Code allophones

A function to classify vowel data into contextual allophones.

## Usage

``` r
code_allophones(
  .df,
  .old_col,
  .new_cols = c("allophone", "allophone_environment"),
  .pre_seg,
  .fol_seg,
  .coronals = c("T", "D", "S", "Z", "SH", "ZH", "JH", "N"),
  .voiceless = c("P", "T", "K", "CH", "F", "TH", "S", "SH")
)
```

## Arguments

- .df:

  The dataset containing vowel data.

- .old_col:

  The unquoted name of the column containing the vowel labels. Often
  called "vowel" or "phoneme" in many datasets. Note that the function
  assumes Wells lexical sets (FLEECE, TRAP, etc.) rather than ARPABET
  (IY, AE, etc.) or IPA (i, æ, etc.). If your vowels are not already
  coded using Wells' labels you can quickly do so with
  `switch_transcriptions` or one of the shortcuts like `arpa_to_wells`

- .new_cols:

  A vector of two strings containing the names of the columns you would
  like to use. By default `c("allophone", "allophone_environment")`. The
  first name becomes the name of the column containing the new allophone
  labels. The second column becomes the name of the column describing
  those labels.

- .pre_seg:

  The unquoted name of the column that contains the labels for the
  previous segement. In DARLA-generated spreadsheets, this is
  \`pre_seg\` and in FastTrack-generated spreadsheets, it's
  \`previous_sound\`. Assumes ARPABET labels.

- .fol_seg:

  The unquoted name of the column that contains the labels for the
  following segement. In DARLA-generated spreadsheets, this is
  \`fol_seg\` and in FastTrack-generated spreadsheets, it's
  \`next_sound\`. Assumes ARPABET labels.

- .coronals:

  A vector of strings containing ARPABET labels for coronal consonants.
  By default, `c("T", "D", "S", "Z", "SH", "ZH", "JH", "N")`. This is
  used to create the \`TOOT\` allophone of \`GOOSE\`.

- .voiceless:

  A vector of strings containing ARPABET labels for voiceless
  consonants. By default,
  `c("P", "T", "K", "CH", "F", "TH", "S", "SH")`. This is used to create
  the \`PRICE\` allophone of \`PRIZE\`.

## Value

A dataframe with two additional columns. One column contains labels for
the allophones and the other contains category labels for those
allophones' contexts. The second column can be useful for quickly
excluding certain allophones like prelaterals or prerhotics or coloring
families of allophones in visualizations (such as turning all prelateral
allophones gray). These two new columns are positioned immediately after
the original vowel column indicated in `.old_col`,

## Note

Here are the list of the contextual allophones that are created. Note
that I largely follow my own advice about what to call [elsewhere
allophones](https://joeystanley.com/blog/why-do-people-use-bat-instead-of-trap),
what to call [prelateral
allophones](https://joeystanley.com/blog/extending-wells-lexical-sets-to-prelateral-vowels),
and [other
allophones](https://joeystanley.com/blog/thoughts-on-allophonic-extensions-to-wells-lexical-sets).
Obviously, this list is pretty subjective and largely based on what my
own research has needed, so it may not work completely for you and your
research. Please contact me at <joey_stanley@byu.edu> if you want to see
an allophone get added or if you spot an error in the coding.

- FLEECE becomes

  - ZEAL before laterals

  - BEET elsewhere

- KIT becomes

  - GUILT before laterals

  - NEAR before rhotics

  - BIG before G

  - BIN before M and N

  - BING before NG

  - BIT elsewhere

- FACE becomes

  - FLAIL before laterals

  - VAGUE before G

  - BAIT elsewhere

- DRESS becomes

  - SHELF before laterals

  - SQUARE before rhotics

  - BEG before G

  - BEN before M and N

  - BENG before NG

  - BET elsewhere

- TRAP becomes

  - TALC before laterals

  - BAG before G

  - BAN before M and N

  - BANG before NG

  - BAT elsewhere

- LOT becomes

  - GOLF before laterals

  - START before rhotics

  - BOT elsewhere

- THOUGHT becomes

  - FAULT before laterals

  - FORCE befpre rhotics

  - BOUGHT elsewhere

- STRUT becomes

  - MULCH before laterals

  - BUT elsewhere

- GOAT becomes

  - JOLT before laterals

  - BOAT elsewhere

- FOOT becomes

  - WOLF before laterals

  - CURE before rhotics

  - PUT elsewhere

- GOOSE becomes

  - MULE before Y

  - SPOOL before laterals

  - CURE before rhotics

  - TOOT before coronals

  - BOOT elsewhere

- PRICE becomes

  - PRICE before voiceless segments

  - PRIZE elsewhere

Unfortunately, it is not straightforward to customize this list but you
can always copy the source code and modify the list yourself.

Alternatively, you can use `forcats::fct_collapse()` to collapse
distinctions that you don't need. See example code below.

You can also of course create your own allophones if desired. Note that
some allophones depend on other environmental information like syllable
structure and morpheme/word boundaries, or they may be entirely lexical
(FORCE vs. NORTH). They may be more complicated than what ARPABET can
code for (MARY, MERRY, and MARRY) or just inconsistently coded. For the
sake of simplicity, these allophones are not included in this function.

The environments therefore are the following

- "prelateral" includes ZEAL, GUILT, FLAIL, SHELF, TALC, GOLF, FAULT,
  MULCH, JOLT, WOLF, SPOOL, CHILD, PROWL

- "prerhotic" includes NEAR, SQUARE, START, FORCE, CURE, PRIOR

- "prevelar" includes BIG, VAGUE, BEG, BAG,

- "prenasal" includes BIN, BEN, BAN

- "prevelarnasal" includes BING, BENG, BANG

- "prevoiceless" includes BITE

- "post-Y" includes MULE

- "postcoronal" includes TOOT

- "elsewhere" includes BEET, BIT, BAIT, BET, BAT, BOT, BOUGHT, BUT,
  BOAT, PUT, BOOT, BIDE, BOUT

## Examples

``` r
suppressPackageStartupMessages(library(tidyverse))
#> Error in library(tidyverse): there is no package called ‘tidyverse’

# Get some sample DARLA data to play with
darla <- joeysvowels::darla %>%
  select(word, vowel, pre_seg, fol_seg) %>%
  mutate(phoneme = joeyr:::arpa_to_wells(vowel), .after = vowel)
#> Error in joeysvowels::darla %>% select(word, vowel, pre_seg, fol_seg) %>%     mutate(phoneme = joeyr:::arpa_to_wells(vowel), .after = vowel): could not find function "%>%"

# Basic usage
darla %>%
  code_allophones(.old_col = phoneme, .fol_seg = fol_seg, .pre_seg = pre_seg) %>%
  slice_sample(n = 20)
#> Error in darla %>% code_allophones(.old_col = phoneme, .fol_seg = fol_seg,     .pre_seg = pre_seg) %>% slice_sample(n = 20): could not find function "%>%"

# Specify the names of the new columns with the `.new_cols` argument
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  slice_sample(n = 20)
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg) %>%     slice_sample(n = 20): could not find function "%>%"

# Filtering by environment is straightforward
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  filter(environment == "elsewhere") %>%
  slice_sample(n = 20)
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg) %>%     filter(environment == "elsewhere") %>% slice_sample(n = 20): could not find function "%>%"
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  filter(!environment %in% c("prerhotic", "prevelarnasal", "prevelar")) %>%
  slice_sample(n = 20)
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg) %>%     filter(!environment %in% c("prerhotic", "prevelarnasal",         "prevelar")) %>% slice_sample(n = 20): could not find function "%>%"

# Some users may want to supply their own list of coronal consonants.
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg,
                  .coronals = c("T", "D", "S", "Z", "SH", "ZH", "JH", "N", "Y")) %>%
  filter(phoneme == "GOOSE") %>%
  slice_sample(n = 20)
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg, .coronals = c("T",     "D", "S", "Z", "SH", "ZH", "JH", "N", "Y")) %>% filter(phoneme ==     "GOOSE") %>% slice_sample(n = 20): could not find function "%>%"

# Other users may want to specify their own list of voiceless consonants.
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg,
                  .voiceless = c("P", "T", "K", "CH", "F", "TH", "S", "SH", "X")) %>%
  filter(phoneme == "PRICE") %>%
  slice_sample(n = 20)
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg, .voiceless = c("P",     "T", "K", "CH", "F", "TH", "S", "SH", "X")) %>% filter(phoneme ==     "PRICE") %>% slice_sample(n = 20): could not find function "%>%"

# Collapsing distinctions can be done post hoc (though it may take extra work to get the environment column to match.)
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  # Get a subset for demonstration purposes
  filter(allophone %in% c("BIT", "BIG")) %>%
  group_by(allophone) %>%
  slice_sample(n = 5) %>%
  ungroup() %>%
  # Now collapse distinctions
  mutate(allophone = fct_collapse(allophone, "BIT" = c("BIT", "BIG")),
         environment = ifelse(allophone == "BIT", "elsewhere", allophone))
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg) %>%     filter(allophone %in% c("BIT", "BIG")) %>% group_by(allophone) %>%     slice_sample(n = 5) %>% ungroup() %>% mutate(allophone = fct_collapse(allophone,     BIT = c("BIT", "BIG")), environment = ifelse(allophone ==     "BIT", "elsewhere", allophone)): could not find function "%>%"

# Creating new allophones depends on the complexity of the allophone
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  # Create voice and voiceless distinctions for MOUTH
  mutate(allophone = case_when(phoneme == "MOUTH" & fol_seg %in% c("P", "T", "K", "CH", "F", "TH", "S", "SH") ~ "BOUT",
                               phoneme == "MOUTH" ~ "LOUD",
                               TRUE ~ allophone),
         environment = if_else(allophone == "BOUT",  "prevoiceless", environment)) %>%
  # Get a subset for demonstration purposes
  filter(phoneme == "MOUTH") %>%
  group_by(allophone) %>%
  slice_sample(n = 5) %>%
  ungroup()
#> Error in darla %>% code_allophones(.old_col = phoneme, .new_cols = c("allophone",     "environment"), .fol_seg = fol_seg, .pre_seg = pre_seg) %>%     mutate(allophone = case_when(phoneme == "MOUTH" & fol_seg %in%         c("P", "T", "K", "CH", "F", "TH", "S", "SH") ~ "BOUT",         phoneme == "MOUTH" ~ "LOUD", TRUE ~ allophone), environment = if_else(allophone ==         "BOUT", "prevoiceless", environment)) %>% filter(phoneme ==     "MOUTH") %>% group_by(allophone) %>% slice_sample(n = 5) %>%     ungroup(): could not find function "%>%"
```
