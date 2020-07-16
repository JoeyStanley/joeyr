
#' Barks and Hertz
#' 
#' @description
#' \code{bark()} and \code{hz()} have been removed from \code{joeyr}. Please use the new package, \code{barktools},
#' 
#' \code{remotes::install_github("joeystanley/barktools")}
#' 
#' to access these functions. These will be phased out in future versions of \code{joeyr}.
#' 
bark <- function() {
  stop('`bark()` and `hz()` have been removed from joeyr. Please use the new package, barktools,
       
  > remotes::install_github("joeystanley/barktools")
       
  to access these functions. These will be phased out in future versions of joeyr.')
}

#' @rdname bark
hz <- bark



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
