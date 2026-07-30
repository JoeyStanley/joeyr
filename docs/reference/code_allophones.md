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

Alternatively, you can use
[`forcats::fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html)
to collapse distinctions that you don't need. See example code below.

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

# Get some sample DARLA data to play with
darla <- joeysvowels::darla %>%
  select(word, vowel, pre_seg, fol_seg) %>%
  mutate(phoneme = joeyr:::arpa_to_wells(vowel), .after = vowel)

# Basic usage
darla %>%
  code_allophones(.old_col = phoneme, .fol_seg = fol_seg, .pre_seg = pre_seg) %>%
  slice_sample(n = 20)
#>            word vowel phoneme allophone allophone_environment pre_seg fol_seg
#> 1         BRIAN    AY   PRICE      BIDE             elsewhere       R     AH0
#> 2           PUT    UH    FOOT       PUT             elsewhere       P       T
#> 3        REAGAN    IY  FLEECE      BEET             elsewhere       R       G
#> 4       BALANCE    AE    TRAP      TALC            prelateral       B       L
#> 5       HIGHEST    AY   PRICE      BIDE             elsewhere      HH     AH0
#> 6          KIDS    IH     KIT       BIT             elsewhere       K       D
#> 7         CROWN    AW   MOUTH      BOUT             elsewhere       R       N
#> 8        LOADED    OW    GOAT      BOAT             elsewhere       L       D
#> 9      INVOLVED    AA     LOT      GOLF            prelateral       V       L
#> 10     NORTHERN    AO THOUGHT     FORCE             prerhotic       N       R
#> 11           Q.    UW   GOOSE      MULE                post-Y       Y        
#> 12 SUBSEQUENTLY    AH   STRUT       BUT             elsewhere       S       B
#> 13         GAME    EY    FACE      BAIT             elsewhere       G       M
#> 14       COTTON    AO THOUGHT    BOUGHT             elsewhere       K       T
#> 15      EXISTED    AH   STRUT       BUT             elsewhere       T       D
#> 16         WIRE    AY   PRICE     PRIOR             prerhotic       W       R
#> 17            A    EY    FACE      BAIT             elsewhere     IY1      CH
#> 18         MORE    AO THOUGHT     FORCE             prerhotic       M       R
#> 19      RAINBOW    EY    FACE      BAIT             elsewhere       R       N
#> 20     INTERPOL    IH     KIT       BIN              prenasal     IY1       N

# Specify the names of the new columns with the `.new_cols` argument
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  slice_sample(n = 20)
#>             word vowel phoneme allophone environment pre_seg fol_seg
#> 1            SEE    IY  FLEECE      BEET   elsewhere       S       W
#> 2        BALLOON    AH   STRUT     MULCH  prelateral       B       L
#> 3     CRITICALLY    IY  FLEECE      BEET   elsewhere       L     IH0
#> 4           HALF    AE    TRAP       BAT   elsewhere      HH       F
#> 5         AROUND    ER   NURSE     NURSE   elsewhere       S     AW1
#> 6             GO    OW    GOAT      BOAT   elsewhere       G     AH0
#> 7          ABOUT    AW   MOUTH      BOUT   elsewhere       B       T
#> 8         THIRTY    ER   NURSE     NURSE   elsewhere      TH       T
#> 9       OVERLAND    ER   NURSE     NURSE   elsewhere       V       L
#> 10      ELEPHANT    AH   STRUT       BUT   elsewhere       F       N
#> 11           HER    ER   NURSE     NURSE   elsewhere      HH      CH
#> 12 RELATIONSHIPS    IH     KIT       BIT   elsewhere      SH       P
#> 13            ON    AO THOUGHT    BOUGHT   elsewhere       T       N
#> 14      ACTUALLY    AE    TRAP       BAT   elsewhere       T       K
#> 15         LEAST    IY  FLEECE      BEET   elsewhere       L       S
#> 16       SOUNDED    AW   MOUTH      BOUT   elsewhere       S       N
#> 17    DISRUPTION    AH   STRUT       BUT   elsewhere       R       P
#> 18      LEARNING    ER   NURSE     NURSE   elsewhere       L       N
#> 19       COUNTRY    AH   STRUT       BUT   elsewhere       K       N
#> 20       FEDERAL    EH   DRESS       BET   elsewhere       F       D

# Filtering by environment is straightforward
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  filter(environment == "elsewhere") %>%
  slice_sample(n = 20)
#>           word vowel phoneme allophone environment pre_seg fol_seg
#> 1         FISH    IH     KIT       BIT   elsewhere       F      SH
#> 2    NECESSARY    IY  FLEECE      BEET   elsewhere       R       F
#> 3        FIFTY    IY  FLEECE      BEET   elsewhere       T       F
#> 4      CASSIDY    AE    TRAP       BAT   elsewhere       K       S
#> 5        THOSE    OW    GOAT      BOAT   elsewhere      DH       Z
#> 6         PALM    AA     LOT       BOT   elsewhere       P       M
#> 7        FOUND    AW   MOUTH      BOUT   elsewhere       F       N
#> 8        ABOUT    AW   MOUTH      BOUT   elsewhere       B       T
#> 9       SHOULD    UH    FOOT       PUT   elsewhere      SH       D
#> 10          T.    IY  FLEECE      BEET   elsewhere       T       K
#> 11 COMMUNICATE    EY    FACE      BAIT   elsewhere       K       T
#> 12        VAST    AE    TRAP       BAT   elsewhere       V       S
#> 13      SETTLE    EH   DRESS       BET   elsewhere       S       T
#> 14        TURN    ER   NURSE     NURSE   elsewhere       T       N
#> 15     LESSONS    AH   STRUT       BUT   elsewhere       S       N
#> 16     REGULAR    ER   NURSE     NURSE   elsewhere       L       S
#> 17        TEXT    EH   DRESS       BET   elsewhere       T       K
#> 18    SILENCED    AH   STRUT       BUT   elsewhere       L       N
#> 19     GALLERY    ER   NURSE     NURSE   elsewhere       L     IY0
#> 20         PUT    UH    FOOT       PUT   elsewhere       P       T
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg) %>%
  filter(!environment %in% c("prerhotic", "prevelarnasal", "prevelar")) %>%
  slice_sample(n = 20)
#>           word vowel phoneme allophone  environment pre_seg fol_seg
#> 1       FLIGHT    AY   PRICE      BITE prevoiceless       L       T
#> 2       LOOKED    UH    FOOT       PUT    elsewhere       L       K
#> 3     GUESSING    EH   DRESS       BET    elsewhere       G       S
#> 4           IN    IH     KIT       BIN     prenasal               N
#> 5           TO    UW   GOOSE      TOOT  postcoronal       T       B
#> 6           TO    UW   GOOSE      TOOT  postcoronal       T       N
#> 7       ARCHIE    IY  FLEECE      BEET    elsewhere      CH     AY1
#> 8         ALSO    OW    GOAT      BOAT    elsewhere       S       W
#> 9      COUNTER    ER   NURSE     NURSE    elsewhere       T     AH0
#> 10        PASS    AE    TRAP       BAT    elsewhere       P       S
#> 11   COMPOSING    OW    GOAT      BOAT    elsewhere       P       Z
#> 12       GIRLS    ER   NURSE     NURSE    elsewhere       G       L
#> 13          ON    AO THOUGHT    BOUGHT    elsewhere     IY1       N
#> 14       CROSS    AO THOUGHT    BOUGHT    elsewhere       R       S
#> 15         LEE    IY  FLEECE      BEET    elsewhere       L       R
#> 16        WAYS    EY    FACE      BAIT    elsewhere       W       Z
#> 17          MR    ER   NURSE     NURSE    elsewhere       T       M
#> 18 DISCOVERING    AH   STRUT       BUT    elsewhere       K       V
#> 19    RECENTLY    IY  FLEECE      BEET    elsewhere       L       P
#> 20        SKIN    IH     KIT       BIN     prenasal       K       N

# Some users may want to supply their own list of coronal consonants.
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg,
                  .coronals = c("T", "D", "S", "Z", "SH", "ZH", "JH", "N", "Y")) %>%
  filter(phoneme == "GOOSE") %>%
  slice_sample(n = 20)
#>        word vowel phoneme allophone environment pre_seg fol_seg
#> 1  STUDENTS    UW   GOOSE      TOOT postcoronal       T       D
#> 2     HUMAN    UW   GOOSE      MULE      post-Y       Y       M
#> 3      INTO    UW   GOOSE      TOOT postcoronal       T     EY1
#> 4       WHO    UW   GOOSE     SPOOL  prelateral      HH       L
#> 5       CUE    UW   GOOSE      MULE      post-Y       Y      DH
#> 6        TO    UW   GOOSE      TOOT postcoronal       T       F
#> 7  NUMEROUS    UW   GOOSE      TOOT postcoronal       N       M
#> 8       USE    UW   GOOSE      MULE      post-Y       Y       Z
#> 9        TO    UW   GOOSE     SPOOL  prelateral       T       L
#> 10       TO    UW   GOOSE      TOOT postcoronal       T      DH
#> 11       TO    UW   GOOSE      TOOT postcoronal       T        
#> 12     INTO    UW   GOOSE      TOOT postcoronal       T       W
#> 13     RULE    UW   GOOSE     SPOOL  prelateral       R       L
#> 14  FORTUNE    UW   GOOSE      BOOT   elsewhere      CH       N
#> 15    GROUP    UW   GOOSE      BOOT   elsewhere       R       P
#> 16       TO    UW   GOOSE      TOOT postcoronal       T       K
#> 17       TO    UW   GOOSE      TOOT postcoronal       T     IH0
#> 18      TWO    UW   GOOSE      TOOT postcoronal       T     ER0
#> 19       TO    UW   GOOSE      TOOT postcoronal       T        
#> 20       TO    UW   GOOSE      TOOT postcoronal       T     AE1

# Other users may want to specify their own list of voiceless consonants.
darla %>%
  code_allophones(.old_col = phoneme,
                  .new_cols = c("allophone", "environment"),
                  .fol_seg = fol_seg,
                  .pre_seg = pre_seg,
                  .voiceless = c("P", "T", "K", "CH", "F", "TH", "S", "SH", "X")) %>%
  filter(phoneme == "PRICE") %>%
  slice_sample(n = 20)
#>          word vowel phoneme allophone  environment pre_seg fol_seg
#> 1     ANXIETY    AY   PRICE      BIDE    elsewhere       Z     AH0
#> 2       PRICE    AY   PRICE      BITE prevoiceless       R       S
#> 3    POLITELY    AY   PRICE      BITE prevoiceless       L       T
#> 4       WHITE    AY   PRICE      BITE prevoiceless       W       T
#> 5        FIND    AY   PRICE      BIDE    elsewhere       F       N
#> 6        I'VE    AY   PRICE      BIDE    elsewhere       K       V
#> 7  IDENTIFIED    AY   PRICE      BIDE    elsewhere     IY0       D
#> 8        I'VE    AY   PRICE      BIDE    elsewhere       T       V
#> 9       WHILE    AY   PRICE     CHILD   prelateral       W       L
#> 10       TIME    AY   PRICE      BIDE    elsewhere       T       M
#> 11     HYBRID    AY   PRICE      BIDE    elsewhere      HH       B
#> 12       LINE    AY   PRICE      BIDE    elsewhere       L       N
#> 13      BLIND    AY   PRICE      BIDE    elsewhere       L       N
#> 14       SIDE    AY   PRICE      BIDE    elsewhere       S       D
#> 15      BRYCE    AY   PRICE      BITE prevoiceless       R       S
#> 16         BY    AY   PRICE      BIDE    elsewhere       B        
#> 17       FIVE    AY   PRICE      BIDE    elsewhere       F       V
#> 18   BRIGHTLY    AY   PRICE      BITE prevoiceless       R       T
#> 19    MICHAEL    AY   PRICE      BITE prevoiceless       M       K
#> 20       FIND    AY   PRICE      BIDE    elsewhere       F       N

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
#> # A tibble: 10 × 7
#>    word        vowel phoneme allophone environment pre_seg fol_seg
#>    <chr>       <chr> <fct>   <fct>     <chr>       <chr>   <chr>  
#>  1 BEGINNING   IH    KIT     BIT       elsewhere   B       G      
#>  2 TRIGGER     IH    KIT     BIT       elsewhere   R       G      
#>  3 EXISTENCE   IH    KIT     BIT       elsewhere   IY0     G      
#>  4 BIG         IH    KIT     BIT       elsewhere   B       G      
#>  5 EXOTIC      IH    KIT     BIT       elsewhere   V       G      
#>  6 HEADED      IH    KIT     BIT       elsewhere   D       D      
#>  7 CULTIVATED  IH    KIT     BIT       elsewhere   T       D      
#>  8 HIS         IH    KIT     BIT       elsewhere   HH      Z      
#>  9 ADMITTING   IH    KIT     BIT       elsewhere   M       T      
#> 10 ARBITRATION IH    KIT     BIT       elsewhere   B       T      

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
#> # A tibble: 10 × 7
#>    word       vowel phoneme allophone environment  pre_seg fol_seg
#>    <chr>      <chr> <fct>   <chr>     <chr>        <chr>   <chr>  
#>  1 WITHOUT    AW    MOUTH   BOUT      prevoiceless TH      T      
#>  2 HOUSE      AW    MOUTH   BOUT      prevoiceless HH      S      
#>  3 OUT        AW    MOUTH   BOUT      prevoiceless T       T      
#>  4 OUT        AW    MOUTH   BOUT      prevoiceless T       T      
#>  5 ABOUT      AW    MOUTH   BOUT      prevoiceless B       T      
#>  6 COUNTY     AW    MOUTH   LOUD      elsewhere    K       N      
#>  7 BOUNDARIES AW    MOUTH   LOUD      elsewhere    B       N      
#>  8 OUR        AW    MOUTH   LOUD      elsewhere    V       R      
#>  9 NOW        AW    MOUTH   LOUD      elsewhere    N       DH     
#> 10 HOWEVER    AW    MOUTH   LOUD      elsewhere    HH      EH1    
```
