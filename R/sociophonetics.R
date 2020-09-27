
#' Measure euclidean distance
#'
#' This function takes the x and y coordinates of two points and finds the euclidean 
#' distance between them. Note that this only works in two-dimensional data: euclidean 
#' distances between points in a three-dimensional space is not currently supported.
#'
#' @param x1,x2,y1,y2 A number
#' @return The distance between the two points (a number).
#' @examples
#' eucl_dist(1,2,1,2)
eucl_dist <- function (x1, x2, y1, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
}


# A custom Pillai function that is tidyverse-compatible (I've used this before). 
# This one takes the same arguments as `manova` but returns just the pillai score rather 
# than the entire model. Requires no additional packages.


#' Calculate Pillai scores
#'
#' This is just a shortcut to run an MANOVA and return just the pillai score. 
#' 
#' @param ... Arguments that may also be passed to `manova()`. Typically a formula and a dataframe.
#' @return The pillai score from the MANOVA test.
#' @examples
#' # Create some artificial vowel data
#' vowel_A <- data.frame(F1 = rnorm(50,  0, 1),
#'                       F2 = rnorm(50, -2, 1),
#'                       vowel = "A")
#' vowel_B <- data.frame(F1 = rnorm(50,  0, 1),
#'                       F2 = rnorm(50,  2, 1),
#'                       vowel = "B")
#' vowels <- rbind(vowel_A, vowel_B)
#' 
#' # Get the pillai score.
#' pillai(cbind(F1, F2) ~ vowel, data = vowels)
pillai <- function(...) {
    # Run the actual test and save it.
    manova_test <- manova(...)
    
    # The pillai score is a part of the summary output, so save that.
    manova_summary <- summary(manova_test)
    
    # Of the several things stored in this list, the statistics table is in "stats" element
    stats <- summary(manova_test)$stats
    
    # The pillai score itself is the first row of the Pillai column
    stats[1,"Pillai"]
}



# This function is a tidyverse-compatible version of the `mahalanobis` function. It just makes it easier to include it as part of a `dplyr::mutate`. One small quirk, if there are fewer than 5 measurements, it returns them all as having a distance of zero. Prevents some errors that way. Requires the MASS package to be install, but does not load it.
#' Calculate Mahalanobis Distance
#'
#' This is a tidyverse-tidyverse-compatible version of the `mahalanobis` function. It just 
#' makes it easier to include it as part of a `dplyr::mutate`. One small modification that 
#' `mahalnobis` function does not do is that if there are fewer than 5 measurements in the
#' dataset, , it returns them all as having a distance of zero. Prevents some errors that 
#' way. Requires the MASS package to be installed, but this this not necessarily loaded. 
#' 
#' @param ... Names of columns that should be included in the Mahalanobis distance. For vowel data, this is typically your F1 and F2 columns.
#' @return A vector that contains the Mahalanobis distances for each observation.
#' @examples
#' require(dplyr)
#' 
#' # Create some artificial vowel data
#' tibble(F1 = rnorm(50, 0, 1),
#'        F2 = rnorm(50, 0, 1)) %>%
#'  # Calculate the Mahalanobis distance
#'  mutate(mahal_dist = tidy_mahalanobis(F1, F2))
#'
#' # Same thing, but using base R: 
#' vowel_data <- data.frame(F1 = rnorm(50, 0, 1),
#'                          F2 = rnorm(50, 0, 1)) 
#' vowel_data$mahal_dist <- tidy_mahalanobis(vowel_data$F1, vowel_data$F2)
#' head(vowel_data)
tidy_mahalanobis <- function(...) {
  variables <- cbind(...)
  
  if (sum(is.na(variables)) != 0)
  {
    stop("Your data has some NAs, which will cause `tidy_mahalnobis` to crash. Try removing NAs before running `tidy_mahalanobis`.")
  }
  
  # Return 0 if there are too few points.
  if(nrow(variables) < 5) {
    return(rep(0, nrow(variables)))
  }
  
  # *Tips hat to Joe Fruehwald via Renwick for this line of code.*
  t_params <- MASS::cov.trob(variables)
  
  
  mahalanobis(variables,
              center = t_params$center,
              cov    = t_params$cov)
}




#' ANAE Vowel Normalization
#' 
#' This is a a tidyverse-compatible function that makes it easy to normalize 
#' your data using the method described in the Atlas of North American English. 
#' NOTE: This is brand new and has not been tested robustly. 
#' 
#' It is unclear how the ANAE function should work with trajectory data.
#' This function pools all data together and normalizes it together, which means
#' one small modification was required to calculate the G value: I had to add
#' the average number of time points per vowel token in the denominator. Not sure
#' if that's how it should be done, but it makes sense to me and returns sensible
#' results. 
#' 
#' @param df The dataframe containing the formant measurements you want to normalize.
#' @param hz_cols A list of columns (unquoted) containing the formant measurements themselves.
#' @param vowel_id The name of the column containing unique identifiers per vowel.
#' @param speaker The name of the column containing unique identifiers per speaker (usually the column containing the speaker name).
#' 
#' @return The same dataframe, but with new column(s), suffixed with "_anae" that have the normalized data.
#' @examples 
#' library(tidyverse)
#' df <- joeysvowels::idahoans
#' 
#' # Run the function. Since this data doesn't have a column for token ids, I'll create one here.
#' df %>%
#'    rowid_to_column("token_id") %>%
#'    norm_anae(hz_cols = c(F1, F2), vowel_id = token_id, speaker = speaker) %>%
#'    select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
norm_anae <- function(df, hz_cols, vowel_id, speaker_id) {
  # Get the sum of log of the hz
  sum_log_hz <- df %>%
    select({{hz_cols}}) %>%
    log() %>%
    sum(na.rm = TRUE)
  
  # Get the number of tokens
  n_tokens <- df %>%
    select({{vowel_id}}) %>%
    distinct() %>%
    nrow()
  
  # Get the number of formants (may be 1 if in a tall format)
  n_formants <- df %>%
    select({{hz_cols}}) %>%
    length()
  
  # Trajectory info is not in the ANAE formula, but I need to add it to the denominator.
  # I can calculate it by dividing the number of measurements by the number of tokens.
  n_timepoints <- nrow(df)/n_tokens
  
  # Now use those to get G
  g <- sum_log_hz / (n_tokens * n_formants * n_timepoints)
  
  # Now use G to get the scaling factors for each speaker
  scaling_factors <- df %>%
    group_by({{speaker_id}}) %>%
    summarize(individual_sum_log_hz = sum(log({{hz_cols}}), na.rm = TRUE),
              individual_n_tokens = n(),
              .groups = "keep") %>% # <- suppresses a warning
    mutate(s = individual_sum_log_hz / (individual_n_tokens * n_formants), # <- I guess I don't need to add n_timepoints here?
           expansion = exp(g - s)) %>%
    ungroup() %>%
    arrange(expansion) %>%
    select({{speaker_id}}, expansion)
  
  # Now join the expansions to the df, multiply the hz values, and clean up (remove the expansion and rearrange columns)
  df %>%
    left_join(scaling_factors, by = as_label(enquo(speaker_id))) %>%
    mutate(across(c({{hz_cols}}), ~.*expansion, .names = "{col}_anae")) %>%
    select(-expansion) %>%
    relocate(ends_with("anae"), .after = c({{hz_cols}}))
}



#' Low-Back-Merger Shift Index
#' 
#' A function to quickly calculate the Low-Back-Merger Shift Index, per Becker (2019).
#' 
#' If you would like to calculate the LBMS Index for each speaker, be sure to 
#' the data beforehand with \code{group_by()} (see the examples). Also, per Becker's
#' (2019) recommendation, it is recommended that you exclude tokens before nasals, 
#' laterals, rhotics, and /g/ before the calculation. It is also recommended to
#' run the function on Lobanov-normalized data.
#' 
#' Note that this is a new function and has not been tested robustly yet.
#' 
#' @param df The dataframe containing the formant measurements you want to base the calculation off of. 
#' @param vowel_col The name of the column containing the name of the vowel (e.g. \code{vowel})
#' @param F1_col The name of the column containing the F1 measurements (e.g. \code{F1_norm})
#' @param F2_col The name of the column containing the F2 measurements (e.g. \code{F2_norm})
#' @param beet A string that indicates which vowels belong to the BEET class of words (e.g. \code{"IY"})
#' @param bit A string that indicates which vowels belong to the BIT class of words (e.g. \code{"IH"})
#' @param bet A string that indicates which vowels belong to the BET class of words (e.g. \code{"EH"})
#' @param bat A string that indicates which vowels belong to the BAT class of words (e.g. \code{"AE"})
#' 
#' @references 
#' Becker, Kara, ed. The Low-Back-Merger Shift: Uniting the Canadian Vowel Shift, 
#' the California Vowel Shift, and Short Front Vowel Shifts across North America. 
#' Publication of the American Dialect Society 104. Durham, NC: Duke University 
#' Press, 2019.
#' 
#' @return A \code{summarize}d dataframe with the LBMS Index.
#' @examples 
#' suppressPackageStartupMessages(library(tidyverse))
#' # This dataset has following segment and normalized data already
#' darla <- joeysvowels::darla 
#' darla %>%
#'   filter(!fol_seg %in% c("L", "R", "M", "N", "NG", "G")) %>%
#'   lbms_index(vowel, F1_LobanovNormed_unscaled, F2_LobanovNormed_unscaled, "IY", "IH", "EH", "AE")
#' 
#' # This dataset has multiple speakers and needs to be normalized
#' idahoans <- joeysvowels::idahoans
#' idahoans %>%
#'   group_by(speaker) %>%
#'   mutate(F1_lob = scale(F1), 
#'          F2_lob = scale(F2)) %>%
#'   lbms_index(vowel, F1_lob, F2_lob, "IY", "IH", "EH", "AE")
lbms_index <- function(df, vowel_col, F1_col, F2_col, beet, bit, bet, bat) {
  df %>%
    filter({{vowel_col}} %in% c(beet, bit, bet, bat)) %>%
    rename(.vowel = {{vowel_col}}, 
           .F1 = {{F1_col}},
           .F2 = {{F2_col}}) %>%
    group_by(.vowel, .add = TRUE) %>%
    summarize(across(c(.F1, .F2), mean), .groups = "keep") %>%
    pivot_wider(names_from = .vowel, values_from = c(.F1, .F2)) %>%
    rowwise() %>%
    mutate(.d_bit = eucl_dist(.F1_IY, .F1_IH, .F2_IY, .F2_IH),
           .d_bet = eucl_dist(.F1_IY, .F1_EH, .F2_IY, .F2_EH),
           .d_bat = eucl_dist(.F1_IY, .F1_AE, .F2_IY, .F2_AE),
           lbms_index = mean(c(.d_bit, .d_bet, .d_bat))) %>%
    select(-starts_with(".F"), -starts_with(".d_")) 
}






#' ∆F Normalization
#' 
#' Normalize vowel formant measurements with ∆F (see Johnson 2020). This 
#' function is intended to be used within a tidyverse pipeline. 
#' 
#' You will need to group the data by speaker with \code{group_by()} before 
#' applying this function if you want to normalize the data by speaker.
#' 
#' The data must be numeric, and there cannot be any NAs. So, if you're using 
#' data extracted from Praat, you may have to filter out bad F3 and F4 data
#' and then convert the column to numeric. See the example.
#' 
#' Note that this is a new function and has not been tested robustly yet.
#' 
#' @param df The data frame containing the formant measurements you want to 
#' normalize
#' @param .F1,.F2,.F3,.F4 The (unquoted) name of the column containing the F1 
#' measurements. The first three are required, but you may leave off F4.
#' 
#' @references 
#' Johnson, Keith. The ΔF Method of Vocal Tract Length Normalization for Vowels. 
#' Laboratory Phonology: Journal of the Association for Laboratory Phonology 11, 
#' no. 1 (July 22, 2020): 10. https://doi.org/10.5334/labphon.196.
#' 
#' @return The original dataframe with new columns (suffixed with 
#' \code{_deltaF}) containing the normalized measurements.
#' @examples 
#' library(tidyverse)
#' df <- joeysvowels::idahoans
#'    
#'  # Run the function
#'  df %>%  
#'    group_by(speaker) %>%
#'    norm_deltaF(F1, F2, F3)
#'    
norm_deltaF <- function(df, .F1, .F2, .F3, .F4) {
  
  # If only three formants are supplied...
  if (missing(.F4)) {
    
    deltaFs <- df %>%
      mutate(.F1_sum = {{.F1}} / 0.5,
             .F2_sum = {{.F2}} / 1.5,
             .F3_sum = {{.F3}} / 2.5) %>%
      mutate(sum_formants = .F1_sum + .F2_sum + .F3_sum) %>%
      summarize(delta_F = sum(sum_formants)/(3 * n()), .groups = "keep")
    
    df %>%
      left_join(deltaFs, by = group_vars(df)) %>%
      mutate(across(c({{.F1}}, {{.F2}}, {{.F3}}), ~./delta_F, .names = "{col}_norm")) %>%
      relocate(ends_with("norm"), .after = {{.F3}}) %>%
      select(-delta_F) %>%
      return()
    
    # If F4 is also supplied...
  } else {
    
    numeric_cols <- df %>%
      summarize(across(c({{.F1}}, {{.F2}}, {{.F3}}, {{.F4}}), is.numeric)) %>%
      t() %>%
      sum()
    if (numeric_cols != 4) {
      warning("F1, F2, F3, and F4 columns must all be numeric. Normalization not completed.")
      return(df)
    }
    
    deltaFs <- df %>%
      mutate(.F1_sum = {{.F1}} / 0.5,
             .F2_sum = {{.F2}} / 1.5,
             .F3_sum = {{.F3}} / 2.5,
             .F4_sum = {{.F4}} / 3.5) %>%
      mutate(sum_formants = .F1_sum + .F2_sum + .F3_sum + .F4_sum) %>%
      summarize(delta_F = sum(sum_formants)/(4 * n()), .groups = "keep")
    
    df %>%
      left_join(deltaFs, by = group_vars(df)) %>%
      mutate(across(c({{.F1}}, {{.F2}}, {{.F3}}, {{.F4}}), ~./delta_F, .names = "{col}_norm")) %>%
      relocate(ends_with("norm"), .after = {{.F4}}) %>%
      select(-delta_F) %>%
      return()
  }
}





#' Convert ARPABET to Lexical Sets
#' 
#' A function to convert ARPABET symbols to lexical set keywords.  
#' 
#' Linguists use different ways to code English vowels in a computer-friendly
#' way. FAVE-Align and MFA use ARPABET, which assigns a two-letter code to each
#' vowel phoneme (IY, IH, EY, EH, etc.). An alternative approach is to use a 
#' keyword denoting a lexical set, whether it be the original Wells keywords 
#' or an alternative using the "B_T" frame. See 
#' \href{https://joeystanley.com/blog/why-do-people-use-bat-instead-of-trap}{this blog post} 
#' for more background.
#' 
#' The original Wells' lexical keywords in this function are FLEECE, KIT, FACE,
#' DRESS, TRAP, LOT, THOUGHT, STRUT, GOAT, FOOT, GOOSE, PRICE, MOUTH, CHOICE,
#' and NURSE.
#' 
#' The lexical set using the B_T frame include BEET, BIT, BAIT, BET, BAT, BOT,
#' BOUGHT, BUT, BOAT, BOOK, BOOT, BITE, BOUT, BOY, and BIRD. 
#' 
#' Note that \code{arpa_to_wells} is shorthand for \code{arpa_to_keywords(style="wells")}, 
#' and only exports to the Wells lexical sets.
#'  
#' @param x The vector containing the vowel labels you want to convert.
#' @param style a string. By default, \code{"Wells"}, which will produce the original 
#' Wells labels. If set to \code{"b_t"}, it will use the "B_T" frame. 
#' @param ordered a logical. by default, \code{TRUE}, which will return the factor in an 
#' order that goes approximately counter clockwise in the vowel space, with 
#' diphthongs last. If \code{FALSE}, it will retain the original order (which, 
#' unless already specified, will be alphabetical or the order in which R sees
#' the individial levels).
#' @param as_character a logical. \code{FALSE} by default, meaning it will return
#' the vector as a factor in the order specified by \code{ordered}. If \code{TRUE}, 
#' it will return the vector as a character vector (and will silently ignore
#' the \code{ordered} argument).
#' 
#' @return A vector with the factors recoded. Any string that is not one of the 
#' following will be silently ignored: IY, IH, EY, EH, AE, AA, AO, AH, OW, UH, 
#' UW, AY, AW, OY, ER.
#' 
#' @examples
#' suppressPackageStartupMessages(library(tidyverse))
#' 
#' darla <- joeysvowels::darla 
#' darla %>%
#'   mutate(vowel = arpa_to_keywords(vowel)) %>%
#'   count(vowel)
#'   
#' darla %>%
#'   mutate(vowel = arpa_to_keywords(vowel, ordered = FALSE)) %>%
#'   count(vowel)
#'
#' darla %>%
#'   mutate(vowel = arpa_to_keywords(vowel, style = "b_t", as_character = TRUE)) %>%
#'   count(vowel)
#'   
#' # Here's a non-tidyverse version (though `stringr` is still used under the hood)
#' darla$vowel <- arpa_to_keywords(darla$vowel)
#' 
arpa_to_keywords <- function(x, style = "wells", ordered = TRUE, as_character = FALSE) {
  style <- tolower(style)
  
  if (ordered) {
    x <- fct_relevel(x, "IY", "IH", "EY", "EH", "AE", "AA", "AO", 
                     "AH", "OW", "UH", "UW", "AY", "AW", "OY", "ER")
  }
  
  if (style == "wells") {
    x <- fct_recode(x,
               "FLEECE"  = "IY",
               "KIT"     = "IH",
               "FACE"    = "EY",
               "DRESS"   = "EH",
               "TRAP"    = "AE",
               "LOT"     = "AA",
               "THOUGHT" = "AO",
               "STRUT"   = "AH",
               "GOAT"    = "OW",
               "FOOT"    = "UH",
               "GOOSE"   = "UW",
               "PRICE"   = "AY",
               "MOUTH"   = "AW",
               "CHOICE"  = "OY",
               "NURSE"   = "ER")
    
  } else if (style == "b_t") {
    x <- fct_recode(x,
               "BEET"   = "IY",
               "BIT"    = "IH",
               "BAIT"   = "EY",
               "BET"    = "EH",
               "BAT"    = "AE",
               "BOT"    = "AA",
               "BOUGHT" = "AO",
               "BUT"    = "AH",
               "BOAT"   = "OW",
               "BOOK"   = "UH",
               "BOOT"   = "UW",
               "BITE"   = "AY",
               "BOUT"   = "AW",
               "BOY"    = "OY",
               "BIRD"   = "ER")
  } else {
    message("Unknown style. Returning original strings.")
  }
  
  if (as_character) {
    x <- as.character(x)
  }
  x
}

#' @rdname arpa_to_keywords
#' @export
arpa_to_wells <- function(x, ...) {
  joeyr::arpa_to_keywords(x, style = "wells", ...)
}