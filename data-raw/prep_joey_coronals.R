## code to prepare `joey_coronals`

library(tidyverse)

# Prep the nonce words ---------------------------------------------------------

# Select onsets (anything that is coronal, or H, or no onset)
pre_segs <- c("t", "d", "s", "z", "n", "h", " ", "st", "sn")

# Select offsets (any voiced coronal obstruent)
fol_segs <- c("d", "z", "dz")

# All vowels
vowels <- c("i", "ɪ", "eɪ", "ɛ", "æ", "a", "ɔ", "oʊ", "ʊ", "u", "aɪ", "aʊ", "ɔɪ")

# Get all combinations of previous segments
preobs <- expand.grid(`pre`   = pre_segs,
                      `vowel` = vowels,
                      `fol`   = fol_segs)

# Export it 
set.seed(1)
wordlist <- bind_rows(preobs, preobs, preobs) %>% # <- each word is in the list 3 times
  unite(word, pre, vowel, fol, sep = "", remove = FALSE) %>%
  rowwise() %>%
  mutate(rand = runif(1)) %>%
  ungroup() %>%
  arrange(rand) %>%
  select(-rand) %>%
  rowid_to_column("id")
head(wordlist)
write_csv(wordlist, "data-raw/wordlist.csv")

# At this point, I opened it in Excel, and recorded myself reading the list in 
# a quiet environment.

# I then used Praat to detect word boundaries with the help of this little Praat
# script:
# 
#     To Intensity: 100, 0, "yes"
#     To TextGrid (silences): -25, 0.1, 0.05, "silent", "sounding"
#
# I then manually checked the word boundaries, manually added vowel boundaries,
# and numbered them sequentially.
# 
# I then extracted formants using the default Praat settings except the number
# of formants was limited to 4 and the max hz was 4500. The resulting 
# spreadsheet was called nonce_CVC_coronals.csv.

# Read in that spreadsheet, remove any missing data.
joey_coronals <- read_csv("data-raw/nonce_CVC_coronals.csv", col_types = cols()) %>%
  mutate(across(c(F3, F4), na_if, "--undefined--"),
         across(c(F3, F4), as.numeric)) %>%
  filter(!is.na(F3), !is.na(F4)) %>%
  
  # Join with the wordlist spreadsheet, which links the sequential numbering 
  # what the word actually was.
  left_join(read_csv("data-raw/wordlist.csv"), by = c("vowel_id" = "id"), col_types = cols()) %>%
  mutate(vowel = fct_recode(vowel, 
                            "FLEECE" = "i", "KIT" = "ɪ", "FACE" = "eɪ", "DRESS" = "ɛ", 
                            "TRAP" = "æ", "LOT" = "a", "THOUGHT" = "ɔ",
                            "GOAT" = "oʊ", "FOOT" = "ʊ", "GOOSE" = "u",
                            "PRICE" = "aɪ", "MOUTH" = "aʊ", "CHOICE" = "ɔɪ")) %>%
  group_by(vowel, percent) %>%
  
  # Remove outliers.
  mutate(is_outlier = find_outliers(F1, F2)) %>%
  filter(!is_outlier) %>%
  select(-is_outlier) %>%
  ungroup() %>%
  print()
write_csv(joey_coronals, "data-raw/joey_coronals.csv")
usethis::use_data(joey_coronals, overwrite = TRUE)

joey_coronals %>%
  pull(vowel_id) %>%
  unique() %>%
  length()
