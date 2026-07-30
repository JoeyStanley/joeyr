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
#> Error in library(tidyverse): there is no package called ‘tidyverse’

darla <- joeysvowels::darla
#> Error in loadNamespace(x): there is no package called ‘joeysvowels’
darla %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = wells)) %>%
  count(vowel)
#> Error: object 'darla' not found

darla %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = wells, ordered = FALSE)) %>%
  count(vowel)
#> Error: object 'darla' not found

darla %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = b_t, as_character = TRUE)) %>%
  count(vowel)
#> Error: object 'darla' not found

# Works even if not all vowel levels are present
darla %>%
  filter(vowel %in% c("IY", "AE", "AY", "UW")) %>%
  mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = b_t)) %>%
  count(vowel)
#> Error: object 'darla' not found

# Here's a non-tidyverse version (though tidyverse is still used under the hood)
darla$vowel <- switch_transcriptions(darla$vowel, .from = arpa, .to = b_t)
#> Error in filter(., {    {        .from    }} %in% x): ℹ In argument: `arpa %in% x`.
#> Caused by error:
#> ! object 'darla' not found

# Note that shortcut functions also exist:
darla %>%
  mutate(vowel = arpa_to_wells(vowel)) %>%
  count(vowel)
#> Error: object 'darla' not found
darla %>%
  mutate(vowel = arpa_to_b_t(vowel)) %>%
  count(vowel)
#> Error: object 'darla' not found
```
