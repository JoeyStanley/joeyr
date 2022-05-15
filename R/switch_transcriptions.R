#' Switch between transcription systems.
#' 
#' A function to switch between ARPABET, Wells' Lexical Sets, the B_T set, and the IPA.
#' 
#' Linguists use different ways to code English vowels in a computer-friendly
#' way. FAVE-Align and MFA use ARPABET, which assigns a two-letter code to each
#' vowel phoneme (IY, IH, EY, EH, etc.). An alternative approach is to use a 
#' keyword denoting a lexical set, whether it be the original Wells keywords 
#' or an alternative using the "B_T" frame. See 
#' \href{https://joeystanley.com/blog/why-do-people-use-bat-instead-of-trap}{this blog post} 
#' for more background.
#' 
#' The ARPABET symbols in this function are IY, IH, EY, EH, AE, AA, AO, AH, OW, UH, 
#' UW, AY, AW, OY, ER.
#' 
#' The original Wells' lexical keywords in this function are FLEECE, KIT, FACE,
#' DRESS, TRAP, LOT, THOUGHT, STRUT, GOAT, FOOT, GOOSE, PRICE, MOUTH, CHOICE,
#' and NURSE.
#' 
#' The lexical set using the B_T frame include BEET, BIT, BAIT, BET, BAT, BOT,
#' BOUGHT, BUT, BOAT, BOOK, BOOT, BITE, BOUT, BOY, and BIRD. 
#' 
#' The IPA symbols include i, ɪ, e, ɛ, æ, ɑ, ɔ, ʌ, o, ʊ, u, ɑɪ, ɑʊ, ɔɪ, and ɚ.
#' 
#' Note that \code{arpa_to_wells} is shorthand for \code{switch_transcriptions(..., .from=arpa, .to=wells)}, 
#' and only exports to the Wells lexical sets. All other pairs of transcription systems have
#' their own shortcut function as well (i.e. \code{wells_to_b_t}, \code{b_t_to_ipa}, \code{ipa_to_wells}, etc.).
#'  
#' @param x The vector containing the vowel labels you want to convert.
#' @param .from an unquoted expression. By default, \code{arpa}, meaning the function
#' will convert ARPABET symbols into another system.
#' @param .to an unquoted expression. By default, \code{wells}, which will produce 
#' the original Wells labels. If set to \code{"b_t"}, it will use the "B_T" frame. 
#' @param ordered a logical. by default, \code{TRUE}, which will return the factor in an 
#' order that goes approximately counter clockwise in the vowel space, with 
#' diphthongs last. If \code{FALSE}, it will retain the original order (which, 
#' unless already specified, will be alphabetical or the order in which R sees
#' the individial levels).
#' @param as_character a logical. \code{FALSE} by default, meaning it will return
#' the vector as a factor in the order specified by \code{ordered}. If \code{TRUE}, 
#' it will return the vector as a character vector (and will silently ignore
#' the \code{ordered} argument).
#' @param warn a logical, \code{TRUE} by default. If there are levels in the vector that
#' are not part of the predefined list above, a warning message will appear alerting 
#' you of that fact. The function will still work, but it's good to be alerted if
#' there is unexpected input. If the \code{ordered} is set to \code{TRUE} then
#' these extra levels will put at the end. This warning can be suppressed by setting
#' this argument to \code{FALSE}.
#' 
#' 
#' @return A vector with the factors recoded. Any string that is not in one of the 
#' preset lists of symbols will remain unchanged.
#' 
#' @examples
#' suppressPackageStartupMessages(library(tidyverse))
#' 
#' darla <- joeysvowels::darla 
#' darla %>%
#'   mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = wells)) %>%
#'   count(vowel)
#'   
#' darla %>%
#'   mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = wells, ordered = FALSE)) %>%
#'   count(vowel)
#'
#' darla %>%
#'   mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = b_t, as_character = TRUE)) %>%
#'   count(vowel)
#'   
#' # Works even if not all vowel levels are present
#' darla %>%
#'   filter(vowel %in% c("IY", "AE", "AY", "UW")) %>%
#'   mutate(vowel = switch_transcriptions(vowel, .from = arpa, .to = b_t)) %>%
#'   count(vowel)
#'   
#' # Here's a non-tidyverse version (though tidyverse is still used under the hood)
#' darla$vowel <- switch_transcriptions(darla$vowel, .from = arpa, .to = b_t)
#' 
#' # Note that shortcut functions also exist:
#' darla %>%
#'   mutate(vowel = arpa_to_wells(vowel)) %>%
#'   count(vowel)
#' darla %>%
#'   mutate(vowel = arpa_to_b_t(vowel)) %>%
#'   count(vowel)
switch_transcriptions <- function(x, .from, .to, ordered = TRUE, as_character = FALSE, warn = TRUE) {
  
  .from_str <- ensyms(.from)
  .to_str   <- ensym(.to)
  if (length(setdiff(c(.from_str, .to_str), c("arpa", "b_t", "ipa", "wells"))) > 0) {
    stop("The only transcriptions currently supported are 'arpa', 'b_t', 'ipa', and 'wells'.")
  }
  
  levels_df <- tibble(
    wells = c("FLEECE", "KIT", "FACE", "DRESS", "TRAP",
              "LOT", "THOUGHT", "STRUT", "GOAT", "FOOT",
              "GOOSE", "PRICE", "MOUTH", "CHOICE", "NURSE"),
    b_t   = c("BEET", "BIT", "BAIT", "BET", "BAT",
              "BOT", "BOUGHT", "BUT", "BOAT", "BOOK",
              "BOOT", "BITE", "BOUT", "BOY", "BIRD"),
    arpa  = c("IY", "IH", "EY", "EH", "AE",
              "AA", "AO", "AH", "OW", "UH",
              "UW", "AY", "AW", "OY", "ER"),
    ipa   = c("i", "ɪ", "e", "ɛ", "æ",
              "ɑ", "ɔ", "ʌ", "o", "ʊ",
              "u", "ɑɪ", "ɑʊ", "ɔɪ", "ɚ")
  ) %>%
    # Only keep the ones present in this dataset
    filter({{.from}} %in% x)
  
  # Warning message about which symbols are not processed
  if (warn) {
    not_processed <- setdiff(x, levels_df %>% pull({{.from}}))
    if (length(not_processed) > 0) {
      warning(paste("The following will be ignored since they are not part of the predefined set:", str_flatten(not_processed, " ")))
    }
  }
  
  levels <- levels_df %>%
    pull({{.from}})
  
  names(levels) <- levels_df %>%
    pull({{.to}})
  
  x <- fct_recode(x, !!!levels)
  
  if (ordered) {
    x <- fct_relevel(x, names(levels))
  }
  
  if (as_character) {
    x <- as.character(x)
  }
  x
}


#' @rdname switch_transcriptions
#' @export
arpa_to_b_t <- function(...) {
  joeyr::switch_transcriptions(.from = arpa, .to = b_t, ...)
}

#' @rdname switch_transcriptions
#' @export
arpa_to_ipa <- function(...) {
  joeyr::switch_transcriptions(.from = arpa, .to = ipa, ...)
}

#' @rdname switch_transcriptions
#' @export
arpa_to_wells <- function(...) {
  joeyr::switch_transcriptions(.from = arpa, .to = wells, ...)
}


#' @rdname switch_transcriptions
#' @export
b_t_to_arpa <- function(...) {
  joeyr::switch_transcriptions(.from = b_t, .to = arpa, ...)
}

#' @rdname switch_transcriptions
#' @export
b_t_to_ipa <- function(...) {
  joeyr::switch_transcriptions(.from = b_t, .to = ipa, ...)
}

#' @rdname switch_transcriptions
#' @export
b_t_to_wells <- function(...) {
  joeyr::switch_transcriptions(.from = b_t, .to = wells, ...)
}

#' @rdname switch_transcriptions
#' @export
ipa_to_arpa <- function(...) {
  joeyr::switch_transcriptions(.from = ipa, .to = arpa, ...)
}

#' @rdname switch_transcriptions
#' @export
ipa_to_b_t <- function(...) {
  joeyr::switch_transcriptions(.from = ipa, .to = b_t, ...)
}

#' @rdname switch_transcriptions
#' @export
ipa_to_wells <- function(...) {
  joeyr::switch_transcriptions(.from = ipa, .to = wells, ...)
}

#' @rdname switch_transcriptions
#' @export
wells_to_arpa <- function(...) {
  joeyr::switch_transcriptions(.from = wells, .to = arpa, ...)
}

#' @rdname switch_transcriptions
#' @export
wells_to_ipa <- function(...) {
  joeyr::switch_transcriptions(.from = wells, .to = ipa, ...)
}

#' @rdname switch_transcriptions
#' @export
wells_to_b_t <- function(...) {
  joeyr::switch_transcriptions(.from = wells, .to = b_t, ...)
}

#' @export
arpa_to_keywords <- function(x, style = "wells", ordered = TRUE, as_character = FALSE) {
  stop("This function has been replaced by the slightly more robust `switch_transcriptions()`. Please switch to that function instead.")
}