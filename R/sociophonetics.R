
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
#' data(joey_formants) # Not the best example because there's only one speaker.
#' joey_formants %>%
#'    rowid_to_column("token_id") %>%
#'    norm_anae(hz_cols = c(F1, F2), vowel_id = token_id, speaker = name)
norm_anae <- function(df, hz_cols, vowel_id, speaker) {
  hz_cols_var  <- enquo(hz_cols)
  vowel_id_var <- enquo(vowel_id)
  speaker_var  <- enquo(speaker)
  
  # Get the sum of log of the hz
  sum_log_hz <- df %>%
    select(!!hz_cols_var) %>%
    log() %>%
    sum(na.rm = TRUE)
  
  # Get the number of tokens
  n_tokens <- df %>%
    select(!!vowel_id_var) %>%
    distinct() %>%
    nrow()
  
  # Get the number of formants (may be 1 if in a tall format)
  n_formants <- length(hz_cols_var)
  
  # Trajectory info is not in the ANAE formula, but I need to add it to the denominator. 
  # I can calculate it by dividing the number of measurements by the number of tokens.
  n_timepoints <- nrow(df)/n_tokens
  
  # Now use those to get G
  g <- sum_log_hz / (n_tokens * n_formants * n_timepoints)
  
  
  # Now use G to get the scaling factors for each speaker
  scaling_factors <- df %>%
    group_by(!!speaker_var) %>%
    summarize(individual_sum_log_hz = sum(log(!!hz_cols_var), na.rm = TRUE),
              individual_n_tokens = n(),
              .groups = "keep") %>% # <- suppresses a warning
    mutate(s = individual_sum_log_hz / (individual_n_tokens * n_formants), # <- I guess I don't need to add n_timepoints here?
           expansion = exp(g - s)) %>%
    ungroup() %>%
    arrange(expansion) %>%
    select(name, expansion)
  
  # Now join the expansions to the df, multiply the hz values, and clean up (remove the expansion and rearrange columns)
  df %>%
    left_join(scaling_factors, by = as_label(speaker_var)) %>%
    mutate(across(c(!!hz_cols_var), ~.*expansion, .names = "{col}_anae")) %>%
    select(-expansion) %>%
    relocate(ends_with("anae"), .after = c(!!hz_cols_var))
}



#' Low-Back-Merger Shift Index
#' 
#' A function to quickly calculate the Low-Back-Merger Shift Index, per Becker (2019).
#' 
#' If you would like to calculate the LBMS Index for each speaker, be sure to 
#' the data beforehand with \code{group_by()} (see the examples). Also, per Becker's
#' (2019) recommendation, it is recommended that you exclude tokens before nasals, 
#' laterals, rhotics, and /g/ before the calculation.
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
#' library(tidyverse)
#' data(joey_formants) # Not the best example because there's only one speaker.
#' joey_formants %>%
#'    filter(!fol_seg %in% c("L", "R", "N", "M", "NG")) %>%
#'    group_by(name) %>%
#'    lbms_index(vowel, F1_LobanovNormed_unscaled, F2_LobanovNormed_unscaled, 
#'               "IY", "IH", "EH", "AE")
#' 
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
#' # Not the best example because there's only one speaker.
#' data(joey_formants) 
#' 
#' # Clean the data first
#' joey_formants_clean <- joey_formants %>%
#'    # Remove tokens without an F3 or F4 measurement
#'    filter(F3 != "None") %>%
#'    # Convert F3 to numeric
#'    mutate(F3 = as.numeric(F3))
#'    
#'  # Run the function
#'  joey_formants_clean %>%  
#'    group_by(name) %>%
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