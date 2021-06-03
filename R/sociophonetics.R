
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
#' A function that runs a MANOVA and returns a summary statistics. For \code{pillai} you get
#' just the pillai score and for \code{manova_p} you get the p-value. It works great 
#' within a tidyverse pipeline, though to be clear, the function itself uses only
#' base R. 
#' 
#' This function is best run when you've got your data properly subsetted so that 
#' only two vowels' data are included. If you want to calculate the pillai score
#' for multiple speakers, you can do so by grouping the data beforehand (using 
#' \code{dplyr::group_by()}). 
#' 
#' If you want to incorporate this function within a tidyverse pipeline, it's best
#' to do so within `dplyr::summarize()` since we are boiling lot of data down to
#' just one number. See the examples below.
#' 
#' Note that because it's just a MANOVA under the hood, you can incorporate whatever
#' variables you want within the model. However, currently, only the pillai score
#' of the first independent variable will be returned. So if you want to include
#' vowel and duration in the model, for example, but you're mostly interested in 
#' the vowel, be sure to put it first in the formula immediately after the tilde.
#' 
#' NAs won't crash the function, but observations with any NAs will be excluded 
#' from analysis. 
#' 
#' @param ... Arguments that may also be passed to \code{manova()}. Typically a formula and a dataframe.
#' @return The pillai score from the MANOVA test.
#' @examples
#' suppressPackageStartupMessages(library(tidyverse))
#' options(dplyr.summarise.inform = FALSE)
#' 
#' # Look at the low back merger in one speaker.
#' one_speaker <- joeysvowels::midpoints
#' one_speaker %>%
#'   filter(vowel %in% c("LOT", "THOUGHT")) %>%
#'   summarize(pillai = pillai(cbind(F1, F2) ~ vowel))
#'   
#' # A non-tidyverse method
#' pillai(cbind(F1, F2) ~ vowel, data = subset(one_speaker, vowel %in% c("LOT", "THOUGHT")))
#' 
#' # Look at the low back merger in many speakers
#' many_speakers <- joeysvowels::idahoans
#' many_speakers %>%
#'   filter(vowel %in% c("AA", "AO")) %>%
#'   group_by(speaker) %>%
#'   summarize(pillai = pillai(cbind(F1, F2) ~ vowel))
#'   
#' # Other variables can be included, but the pillai of only the first independent variable is returned
#' one_speaker %>%
#'   filter(vowel %in% c("LOT", "THOUGHT")) %>%
#'   mutate(dur = end - start) %>%
#'   summarize(pillai = pillai(cbind(F1, F2, F3, F4) ~ vowel + dur))
#' 
#' # Observations with NAs are excluded from the analysis
#' one_speaker[8,]$F1 <- NA
#' one_speaker %>%
#'   filter(vowel %in% c("LOT", "THOUGHT")) %>%
#'   summarize(pillai = pillai(cbind(F1, F2) ~ vowel))
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

#' @rdname pillai
#' @export
manova_p <- function(...) {
  manova_test <- manova(...)
  manova_summary <- summary(manova_test)
  stats <- summary(manova_test)$stats
  stats[1, "Pr(>F)"]
}



# This function is a tidyverse-compatible version of the `mahalanobis` function. It just makes it easier to include it as part of a `dplyr::mutate`. One small quirk, if there are fewer than 5 measurements, it returns them all as having a distance of zero. Prevents some errors that way. Requires the MASS package to be install, but does not load it.
#' Calculate Mahalanobis Distance
#'
#' This is a tidyverse-tidyverse-compatible version of the \code{stats::mahalanobis} function. It just 
#' makes it easier to include it as part of a \code{dplyr::mutate}. 
#' 
#' Typically you'll want to group your data (using \code{dplyr::group_by}) by speaker and vowel 
#' class so that you get the distance from vowel centroids.
#' 
#' I won't tell you what to do with those distances, but if you might consider looking at tokens
#' where the square root of the Mahalanobis distance is greater than around 2. However, to be clear, 
#' the exact cutoff will vary depending on the size and variability of your data. You can see how 
#' you might isolate these points visually in the example code below.
#' 
#' One small modification that this function does that \code{stats::mahalanobis} does not do is that 
#' if there are fewer than 5 measurements in a group, \code{tidy_mahalanobis} returns them all 
#' as having a distance of zero. I found that this prevents some fatal errors from crashing the script
#' when running this function on smaller datasets.
#' 
#' Note that this function requires the \code{MASS} package to be installed to work, but you 
#' don't need to load it.
#' 
#' @param ... Names of columns that should be included in the Mahalanobis distance. For vowel data, this is typically your F1 and F2 columns.
#' @return A vector that contains the Mahalanobis distances for each observation.
#' @examples
#' suppressPackageStartupMessages(library(tidyverse))
#' df <- joeysvowels::midpoints
#' 
#' # Calculate the distances
#' m_dists <- df %>%
#'   group_by(vowel) %>%
#'   mutate(mahal_dist = tidy_mahalanobis(F1, F2))
#'   
#' # Take a peek at the resulting dataset
#' m_dists %>%
#'   select(vowel, F1, F2, mahal_dist) %>%
#'   head()
#'   
#' # Plot potential outliers
#' ggplot(m_dists, aes(F2, F1, color = sqrt(mahal_dist) > 2)) + 
#'    geom_point() + 
#'    scale_x_reverse() +
#'    scale_y_reverse()
#'    
#' # You can include whatever numeric variables you want, like duration
#' df %>%
#'   group_by(vowel) %>%
#'   mutate(dur = end - start) %>%
#'   mutate(mahal_dist = tidy_mahalanobis(F1, F2, dur)) %>%
#'   ggplot(aes(F2, F1, color = sqrt(mahal_dist) > 2.5)) + 
#'   geom_point() + 
#'   scale_x_reverse() +
#'   scale_y_reverse()
#' 
#' # Data cannot contain NAs. Remove them before running.
#' df[1,]$F1 <- NA
#' df %>%
#'   group_by(vowel) %>%
#'   mutate(mahal_dist = tidy_mahalanobis(F1, F2))
#' df %>%
#'   group_by(vowel) %>%
#'   filter(!is.na(F1)) %>%
#'   mutate(mahal_dist = tidy_mahalanobis(F1, F2)) %>%
#'   select(vowel_id, vowel, mahal_dist, F1, F2) %>%
#'   head()
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
#' The data must be grouped by speaker prior to running the function.
#' 
#' The function works best when only F1 and F2 data are included. F3 can be included
#' but the results may not be comparable with other studies. 
#' 
#' By default, the function will use the Telsur G value listed in the ANAE (6.896874).
#' This is recommended to make results most compatible with the ANAE and other
#' studies that use the same normalization procedure. The function can calculate
#' a G value based on the dataset provided when \code{use_telsur_g} is set to 
#' \code{FALSE}. 
#' 
#' It is unclear how the ANAE function should work with trajectory data.
#' This function pools all data together and normalizes it together, which means
#' one small modification was required to calculate the G value if the Telsur G 
#' is not used: I had to add
#' the average number of time points per vowel token in the denominator. Not sure
#' if that's how it should be done, but it makes sense to me and returns sensible
#' results. 
#' 
#' @param df The dataframe containing the formant measurements you want to normalize.
#' @param hz_cols A list of columns (unquoted) containing the formant measurements themselves.
#' @param vowel_id The name of the column containing unique identifiers per vowel token. 
#' If your data is set up so that there is one row per token, you can put \code{row.names(.)}
#' here instead.
#' @param speaker_id The name of the column containing unique identifiers per speaker (usually the column containing the speaker name).
#' @param use_telsur_g By default, this will use the Telsur G value (6.896874) listed in the ANAE. If set to \code{FALSE}, it will calculate the G value based on the dataset.
#' 
#' @return The same dataframe, but with new column(s), suffixed with "_anae" that have the normalized data.
#' @examples 
#' library(tidyverse)
#' df <- joeysvowels::idahoans
#' 
#' # Run the function. Since this data doesn't have a column for token ids, I'll create one here.
#' df %>%
#'    group_by(speaker) %>%
#'    norm_anae(hz_cols = c(F1, F2), vowel_id = row.names(.), speaker_id = speaker) %>%
#'    ungroup() %>%
#'    select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
norm_anae <- function(df, hz_cols, vowel_id, speaker_id, use_telsur_g = TRUE) {
  
  # Get the number of formants (may be 1 if in a tall format)
  n_formants <- df %>%
    ungroup() %>%
    select({{hz_cols}}) %>%
    length()
  
  if (use_telsur_g) {
    g <- 6.896874
  } else {
    # Get the sum of log of the hz
    sum_log_hz <- df %>%
      ungroup() %>%
      select({{hz_cols}}) %>%
      log() %>%
      sum(na.rm = TRUE)
    
    # Get the number of tokens
    n_tokens <- df %>%
      ungroup() %>%
      select({{vowel_id}}) %>%
      distinct() %>%
      nrow()
    
    # Trajectory info is not in the ANAE formula, but I need to add it to the denominator.
    # I can calculate it by dividing the number of measurements by the number of tokens.
    n_timepoints <- nrow(df)/n_tokens
    
    # Now use those to get G
    g <- sum_log_hz / (n_tokens * n_formants * n_timepoints)
  }
  
  # Now use G to get the scaling factors for each speaker
  scaling_factors <- df %>%
    summarize(individual_sum_log_hz = sum(log({{hz_cols}}), na.rm = TRUE),
              individual_n_tokens = n(),
              .groups = "keep") %>% # <- suppresses a warning
    mutate(s = individual_sum_log_hz / (individual_n_tokens * n_formants), # <- I guess I don't need to add n_timepoints here?
           expansion = exp(g - s)) %>%
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
    mutate(.vowel = fct_recode(.vowel,
                               "IY" = beet,
                               "IH" = bit,
                               "EH" = bet,
                               "AE" = bat)) %>%
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
#' The ∆F is a normalization technique that is based on a single, interpretable
#' parameter for each speaker. The parameter is called ∆F and is "an estimate of 
#' formant spacing in a vocal tract with no constrictions" (Johnson 2020:10). 
#' 
#' You will need to group the data by speaker with \code{group_by()} before 
#' applying this function if you want to normalize the data by speaker.
#' 
#' The data must be numeric, and there cannot be any \code{NA}s. So, if you're using 
#' data extracted from Praat, you may have to filter out bad F3 and F4 data
#' and then convert the column to numeric.
#' 
#' Note that this is a new function and has not been tested very robustly yet.
#' 
#' 
#' @param df The data frame containing the formant measurements you want to 
#' normalize
#' @param .F1,.F2,.F3,.F4 The (unquoted) name of the column containing the F1 
#' measurements. The first three are required, but you may leave off F4. It is 
#' recommended that you include F4 if the data is available and reliable since 
#' it produces more accurate results.
#' @param suffix A string. The suffix you'd like to append to column names in 
#' new normalized columns. By default, it's \code{"_deltaF"} so if your original F1 
#' column was called \code{F1} then the normalized one will be \code{F1_deltaF}.
#' @param return A string. By default, it's \code{"formants"} so it'll return the 
#' normalized values for you. If you'd like to see the actual ΔF values, you 
#' can do so by putting \code{"deltaF"} instead.
#' 
#' @references 
#' Johnson, Keith. The ΔF Method of Vocal Tract Length Normalization for Vowels. 
#' Laboratory Phonology: Journal of the Association for Laboratory Phonology 11, 
#' no. 1 (July 22, 2020): 10. https://doi.org/10.5334/labphon.196.
#' 
#' @return The original dataframe with new columns containing the normalized measurements.
#' @examples 
#' library(tidyverse)
#' df <- joeysvowels::idahoans
#'  
#' # Basic usage
#' df %>%  
#'    group_by(speaker) %>%
#'    norm_deltaF(F1, F2, F3, F4)
#'    
#' # F4 is not required
#' df %>%  
#'    group_by(speaker) %>%
#'    norm_deltaF(F1, F2, F3)
#'  
#' # Change the new columns' suffix
#' df %>%  
#'    group_by(speaker) %>%
#'    norm_deltaF(F1, F2, F3, suffix = "_norm")
#'    
#' # Return ∆F instead
#' df %>%  
#'    group_by(speaker) %>%
#'    norm_deltaF(F1, F2, F3, F4, return = "deltaF")
#'    
norm_deltaF <- function(df, .F1, .F2, .F3, .F4, suffix = "_deltaF", return = "formants") {
  
  # If only three formants are supplied...
  if (missing(.F4)) {
    
    deltaFs <- df %>%
      mutate(.F1_sum = {{.F1}} / 0.5,
             .F2_sum = {{.F2}} / 1.5,
             .F3_sum = {{.F3}} / 2.5) %>%
      mutate(sum_formants = .F1_sum + .F2_sum + .F3_sum) %>%
      summarize(deltaF = sum(sum_formants)/(3 * n()), .groups = "keep")
    
    formants <- df %>%
      left_join(deltaFs, by = group_vars(df)) %>%
      mutate(across(c({{.F1}}, {{.F2}}, {{.F3}}), ~./deltaF, .names = paste0("{col}", suffix))) %>%
      relocate(ends_with(suffix), .after = {{.F3}}) %>%
      select(-deltaF)
    
    # If F4 is also supplied...
  } else {
    
    deltaFs <- df %>%
      mutate(.F1_sum = {{.F1}} / 0.5,
             .F2_sum = {{.F2}} / 1.5,
             .F3_sum = {{.F3}} / 2.5,
             .F4_sum = {{.F4}} / 3.5) %>%
      mutate(sum_formants = .F1_sum + .F2_sum + .F3_sum + .F4_sum) %>%
      summarize(deltaF = sum(sum_formants)/(4 * n()), .groups = "keep")
    
    
    formants <- df %>%
      left_join(deltaFs, by = group_vars(df)) %>%
      mutate(across(c({{.F1}}, {{.F2}}, {{.F3}}, {{.F4}}), ~./deltaF, .names = paste0("{col}", suffix))) %>%
      relocate(ends_with(suffix), .after = {{.F4}}) %>%
      select(-deltaF)
  }
  
  if (return == "deltaF") { 
    return(deltaFs)
  } else {
    return(formants)
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
#' and only exports to the Wells lexical sets. \code{wells_to_arpa} is the reverse function
#' and converts Wells lexical sets back into ARPABET.
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
#' # Works even if not all vowel levels are present
#' darla %>%
#'   filter(vowel %in% c("IY", "AE", "AY", "UW")) %>%
#'   mutate(vowel = arpa_to_keywords(vowel)) %>%
#'   count(vowel)
#'   
#' # Here's a non-tidyverse version (though tidyverse is still used under the hood)
#' darla$vowel <- arpa_to_keywords(darla$vowel)
arpa_to_keywords <- function(x, style = "wells", ordered = TRUE, as_character = FALSE) {
  style <- tolower(style)
  
  levels_df <- tibble(
    wells = c("FLEECE", "KIT", "FACE", "DRESS", "TRAP",
              "LOT", "THOUGHT", "STRUT", "GOAT", "FOOT", 
              "GOOSE", "PRICE", "MOUTH", "CHOICE", "NURSE"),
    b_t   = c("BEET", "BIT", "BAIT", "BET", "BAT",
              "BOT", "BOUGHT", "BUT", "BOAT", "BOOK", 
              "BOOT", "BITE", "BOUT", "BOY", "BIRD"),
    arpa  = c("IY", "IH", "EY", "EH", "AE", 
              "AA", "AO", "AH", "OW", "UH", 
              "UW", "AY", "AW", "OY", "ER")
  ) %>%
    # Only keep the ones present in this dataset
    filter(arpa %in% x)
  
  # Used a named vector and then do fct_recode with splice
  levels <- levels_df$arpa
  if (style == "wells") {
    names(levels) <- levels_df$wells
  } else if (style == "b_t") {
    names(levels) <- levels_df$b_t
  } else {
    message("Unknown style. Returning original strings.")
  }
  x <- fct_recode(x, !!!levels)
  
  if (ordered) {
    x <- fct_relevel(x, names(levels))
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

#' @rdname arpa_to_keywords
#' @export
wells_to_arpa <- function(x, ordered = TRUE, as_character = FALSE) {
  
  levels_df <- tibble(
    wells = c("FLEECE", "KIT", "FACE", "DRESS", "TRAP",
              "LOT", "THOUGHT", "STRUT", "GOAT", "FOOT", 
              "GOOSE", "PRICE", "MOUTH", "CHOICE", "NURSE"),
    b_t   = c("BEET", "BIT", "BAIT", "BET", "BAT",
              "BOT", "BOUGHT", "BUT", "BOAT", "BOOK", 
              "BOOT", "BITE", "BOUT", "BOY", "BIRD"),
    arpa  = c("IY", "IH", "EY", "EH", "AE", 
              "AA", "AO", "AH", "OW", "UH", 
              "UW", "AY", "AW", "OY", "ER")
  ) %>%
    # Only keep the ones present in this dataset
    filter(wells %in% x)
  
  # Used a named vector and then do fct_recode with splice
  levels <- levels_df$wells
  names(levels) <- levels_df$arpa
  x <- fct_recode(x, !!!levels)
  
  if (ordered) {
    x <- fct_relevel(x, names(levels))
  }
  
  if (as_character) {
    x <- as.character(x)
  }
  x
}
