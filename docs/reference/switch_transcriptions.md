# Switch between transcription systems.

A function to switch between ARPABET, Wells' Lexical Sets, the B_T set,
and the IPA.

## Usage

``` r
switch_transcriptions(
  x,
  .from,
  .to,
  ordered = TRUE,
  as_character = FALSE,
  warn = TRUE
)

arpa_to_b_t(...)

arpa_to_ipa(...)

arpa_to_wells(...)

b_t_to_arpa(...)

b_t_to_ipa(...)

b_t_to_wells(...)

ipa_to_arpa(...)

ipa_to_b_t(...)

ipa_to_wells(...)

wells_to_arpa(...)

wells_to_ipa(...)

wells_to_b_t(...)
```

## Arguments

- x:

  The vector containing the vowel labels you want to convert.

- .from:

  an unquoted expression. By default, `arpa`, meaning the function will
  convert ARPABET symbols into another system.

- .to:

  an unquoted expression. By default, `wells`, which will produce the
  original Wells labels. If set to `"b_t"`, it will use the "B_T" frame.

- ordered:

  a logical. by default, `TRUE`, which will return the factor in an
  order that goes approximately counter clockwise in the vowel space,
  with diphthongs last. If `FALSE`, it will retain the original order
  (which, unless already specified, will be alphabetical or the order in
  which R sees the individial levels).

- as_character:

  a logical. `FALSE` by default, meaning it will return the vector as a
  factor in the order specified by `ordered`. If `TRUE`, it will return
  the vector as a character vector (and will silently ignore the
  `ordered` argument).

- warn:

  a logical, `TRUE` by default. If there are levels in the vector that
  are not part of the predefined list above, a warning message will
  appear alerting you of that fact. The function will still work, but
  it's good to be alerted if there is unexpected input. If the `ordered`
  is set to `TRUE` then these extra levels will put at the end. This
  warning can be suppressed by setting this argument to `FALSE`.

## Value

A vector with the factors recoded. Any string that is not in one of the
preset lists of symbols will remain unchanged.

## Details

Linguists use different ways to code English vowels in a
computer-friendly way. FAVE-Align and MFA use ARPABET, which assigns a
two-letter code to each vowel phoneme (IY, IH, EY, EH, etc.). An
alternative approach is to use a keyword denoting a lexical set, whether
it be the original Wells keywords or an alternative using the "B_T"
frame. See [this blog
post](https://joeystanley.com/blog/why-do-people-use-bat-instead-of-trap)
for more background.

The ARPABET symbols in this function are IY, IH, EY, EH, AE, AA, AO, AH,
OW, UH, UW, AY, AW, OY, ER.

The original Wells' lexical keywords in this function are FLEECE, KIT,
FACE, DRESS, TRAP, LOT, THOUGHT, STRUT, GOAT, FOOT, GOOSE, PRICE, MOUTH,
CHOICE, and NURSE.

The lexical set using the B_T frame include BEET, BIT, BAIT, BET, BAT,
BOT, BOUGHT, BUT, BOAT, BOOK, BOOT, BITE, BOUT, BOY, and BIRD.

The IPA symbols include i, ɪ, e, ɛ, æ, ɑ, ɔ, ʌ, o, ʊ, u, ɑɪ, ɑʊ, ɔɪ, and
ɚ.

Note that `arpa_to_wells` is shorthand for
`switch_transcriptions(..., .from=arpa, .to=wells)`, and only exports to
the Wells lexical sets. All other pairs of transcription systems have
their own shortcut function as well (i.e. `wells_to_b_t`, `b_t_to_ipa`,
`ipa_to_wells`, etc.).

## Examples

``` r
suppressPackageStartupMessages(library(tidyverse))

darla <- joeysvowels::darla
darla %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = wells)) %>%
  count(vowel)
#>      vowel   n
#> 1   FLEECE 485
#> 2      KIT 390
#> 3     FACE 250
#> 4    DRESS 231
#> 5     TRAP 175
#> 6      LOT 141
#> 7  THOUGHT 244
#> 8    STRUT 283
#> 9     GOAT 285
#> 10    FOOT  55
#> 11   GOOSE 267
#> 12   PRICE 214
#> 13   MOUTH  93
#> 14  CHOICE  13
#> 15   NURSE 378

darla %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = wells, ordered = FALSE)) %>%
  count(vowel)
#>      vowel   n
#> 1      LOT 141
#> 2     TRAP 175
#> 3    STRUT 283
#> 4  THOUGHT 244
#> 5    MOUTH  93
#> 6    PRICE 214
#> 7    DRESS 231
#> 8    NURSE 378
#> 9     FACE 250
#> 10     KIT 390
#> 11  FLEECE 485
#> 12    GOAT 285
#> 13  CHOICE  13
#> 14    FOOT  55
#> 15   GOOSE 267

darla %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = b_t, as_character = TRUE)) %>%
  count(vowel)
#>     vowel   n
#> 1    BAIT 250
#> 2     BAT 175
#> 3    BEET 485
#> 4     BET 231
#> 5    BIRD 378
#> 6     BIT 390
#> 7    BITE 214
#> 8    BOAT 285
#> 9    BOOK  55
#> 10   BOOT 267
#> 11    BOT 141
#> 12 BOUGHT 244
#> 13   BOUT  93
#> 14    BOY  13
#> 15    BUT 283

# Works even if not all vowel levels are present
darla %>%
  filter(vowel %in% c("IY", "AE", "AY", "UW")) %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = b_t)) %>%
  count(vowel)
#>   vowel   n
#> 1  BEET 485
#> 2   BAT 175
#> 3  BOOT 267
#> 4  BITE 214

# Here's a non-tidyverse version (though tidyverse is still used under the hood)
darla$vowel <- switch_transcriptions(darla$vowel, .from = arpa, .to = b_t)

# Note that shortcut functions also exist:
darla %>%
  mutate(vowel = arpa_to_wells(vowel)) %>%
  count(vowel)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `vowel = arpa_to_wells(vowel)`.
#> Caused by warning in `joeyr::switch_transcriptions()`:
#> ! The following will be ignored since they are not part of the predefined set: BITE BIRD BEET BOT BIT BAIT BOAT BOUGHT BOUT BAT BUT BET BOOT BOOK BOY
#>     vowel   n
#> 1    BEET 485
#> 2     BIT 390
#> 3    BAIT 250
#> 4     BET 231
#> 5     BAT 175
#> 6     BOT 141
#> 7  BOUGHT 244
#> 8     BUT 283
#> 9    BOAT 285
#> 10   BOOK  55
#> 11   BOOT 267
#> 12   BITE 214
#> 13   BOUT  93
#> 14    BOY  13
#> 15   BIRD 378
darla %>%
  mutate(vowel = arpa_to_b_t(vowel)) %>%
  count(vowel)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `vowel = arpa_to_b_t(vowel)`.
#> Caused by warning in `joeyr::switch_transcriptions()`:
#> ! The following will be ignored since they are not part of the predefined set: BITE BIRD BEET BOT BIT BAIT BOAT BOUGHT BOUT BAT BUT BET BOOT BOOK BOY
#>     vowel   n
#> 1    BEET 485
#> 2     BIT 390
#> 3    BAIT 250
#> 4     BET 231
#> 5     BAT 175
#> 6     BOT 141
#> 7  BOUGHT 244
#> 8     BUT 283
#> 9    BOAT 285
#> 10   BOOK  55
#> 11   BOOT 267
#> 12   BITE 214
#> 13   BOUT  93
#> 14    BOY  13
#> 15   BIRD 378
```
