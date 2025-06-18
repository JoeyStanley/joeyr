#' ANAE Vowel Normalization
#'
#' This is a a tidyverse-compatible function that makes it easy to normalize
#' your data using the method described in the Atlas of North American English
#' (Labov, Ash, & Boberg 2006).
#'
#' The data must be grouped by speaker prior to running the function.
#'
#' The function works best when only F1 and F2 data are included. F3 can be included
#' but the results may not be comparable with other studies.
#'
#' By default, the function will use the Telsur G value listed in the ANAE (6.896874)
#' which will make the results most compatible with the ANAE and other
#' studies that use the same normalization procedure. The function can calculate
#' a G value based on the dataset provided when \code{g} is set to \code{"calculate"}.
#' Alternatively, \code{g} can be set to an arbitrary number, such as zero.
#'
#' It is unclear how the ANAE function should work with trajectory data.
#' This function pools all data together and normalizes it together, which means
#' one small modification was required to calculate the G value if the Telsur G
#' is not used: I had to add the average number of time points per vowel token
#' in the denominator. Not sure if that's how it should be done, but it makes
#' sense to me and returns sensible results.
#'
#' @note As of June 18, 2025, the `norm_anae` function is depreciated in favor of
#' the various functions in the tidynorm package. I recommend you switch to
#' that for future code. If you want to retain the current functionality, you can
#' call `joeyr_norm_anae` instead.
#'
#' @param df The dataframe containing the formant measurements you want to normalize.
#' @param hz_cols A list of columns (unquoted) containing the formant measurements themselves.
#' @param token_id The name of the column containing unique identifiers per vowel token.
#' If your data is set up so that there is one row per token, you can put \code{row.names(.)}
#' here instead.
#' @param speaker_id The name of the column containing unique identifiers per speaker (usually the column containing the speaker name).
#' @param g By default, \code{"telsur"}, whichwill use the Telsur G value (6.896874)
#' listed in the ANAE. If set to \code{"calculate"}, it will calculate the G
#' value based on the dataset. This can be set to any arbitrary number, such as
#' \code{0} as well.
#'
#' @return The same dataframe, but with new column(s), suffixed with "_anae" that have the normalized data.
#'
#' @references
#' Labov, William, Sharon Ash, and Charles Boberg. \emph{The Atlas of North American
#' English: Phonetics, Phonology and Sound Change.} Berlin: Walter de Gruyter, 2006.

#' @examples
#' library(tidyverse)
#' df <- joeysvowels::idahoans
#'
#' df %>%
#'    group_by(speaker) %>%
#'    norm_anae(hz_cols = c(F1, F2), speaker_id = speaker) %>%
#'    ungroup() %>%
#'    select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
#'
#' # Slightly different if G is calculated internally.
#' df %>%
#'    group_by(speaker) %>%
#'    norm_anae(hz_cols = c(F1, F2), speaker_id = speaker, g = "calculate") %>%
#'    ungroup() %>%
#'    select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
#'
#' # G can be set to an arbitrary value.
#' df %>%
#'    group_by(speaker) %>%
#'    norm_anae(hz_cols = c(F1, F2), speaker_id = speaker, g = 0) %>%
#'    ungroup() %>%
#'    select(F1, F2, F1_anae, F2_anae) # <- just the relevant columns
joeyr_norm_anae <- function(df, hz_cols, token_id, speaker_id, g = "telsur") {

  # Get the number of formants (may be 1 if in a tall format)
  n_formants <- df %>%
    ungroup() %>%
    select({{hz_cols}}) %>%
    length()

  if (g == "telsur") {
    this_g <- 6.896874
  } else if (g == "calculate") {

    # Get the sum of log of the hz
    sum_log_hz <- df %>%
      ungroup() %>%
      select({{hz_cols}}) %>%
      log() %>%
      sum(na.rm = TRUE)

    # Get the number of tokens
    if (missing(token_id)) {
      n_tokens <- nrow(df)
    } else {
      n_tokens <- df %>%
        ungroup() %>%
        select({{token_id}}) %>%
        distinct() %>%
        nrow()
    }

    # Trajectory info is not in the ANAE formula, but I need to add it to the denominator.
    # I can calculate it by dividing the number of measurements by the number of tokens.
    n_timepoints <- nrow(df)/n_tokens

    # Now use those to get G
    this_g <- sum_log_hz / (n_tokens * n_formants * n_timepoints)
  } else if (is.numeric(g)) {
    this_g <- g
  }

  # Now use G to get the scaling factors for each speaker
  scaling_factors <- df %>%
    summarize(individual_sum_log_hz = sum(log({{hz_cols}}), na.rm = TRUE),
              individual_n_tokens = n(),
              .groups = "keep") %>% # <- suppresses a warning
    mutate(s = individual_sum_log_hz / (individual_n_tokens * n_formants), # <- I guess I don't need to add n_timepoints here?
           expansion = exp(this_g - s)) %>%
    arrange(expansion) %>%
    select({{speaker_id}}, expansion)

  # Now join the expansions to the df, multiply the hz values, and clean up (remove the expansion and rearrange columns)
  df %>%
    left_join(scaling_factors, by = as_label(enquo(speaker_id))) %>%
    mutate(across(c({{hz_cols}}), ~.*expansion, .names = "{col}_anae")) %>%
    select(-expansion) %>%
    relocate(ends_with("anae"), .after = c({{hz_cols}}))
}

#' @rdname joeyr_norm_anae
#' @export
norm_anae <- function(.df) {
  warning("As of June 18, 2025 (joeyr version 0.10), the `norm_anae` function has been depreciated in the {{joeyr}} package. I recommend you switch to the {{tidynorm}} package by Josef Fruehwald. See the package website here (https://jofrhwld.github.io/tidynorm/) and an introduction to the package here (https://jofrhwld.github.io/blog/posts/2025/06/2025-06-16_introducing-tidynorm/). If you do not want to make the switch and would like to use the {{joeyr}} version instead, you still can by calling `joeyr_norm_anae()` instead.")
  return(.df)
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
#' @note As of June 18, 2025, the `norm_deltaF` function is depreciated in favor of
#' the function of the same name in the tidynorm package. I recommend you switch to
#' that for future code. If you want to retain the current functionality, you can
#' call `joeyr_norm_deltaF` instead.
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
#' Johnson, Keith. 2020. The ΔF Method of Vocal Tract Length Normalization for Vowels.
#' \emph{Laboratory Phonology: Journal of the Association for Laboratory
#' Phonology} 11(1).
#' \href{https://doi.org/10.5334/labphon.196}{https://doi.org/10.5334/labphon.196}.
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
joeyr_norm_deltaF <- function(df, .F1, .F2, .F3, .F4, suffix = "_deltaF", return = "formants") {

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
#' @rdname joeyr_norm_deltaF
#' @export
norm_deltaF <- function(.df) {
  warning("As of June 18, 2025 (joeyr version 0.10), the `norm_deltaF` function has been depreciated in the {{joeyr}} package. I recommend you switch to the {{tidynorm}} package by Josef Fruehwald. See the package website here (https://jofrhwld.github.io/tidynorm/) and an introduction to the package here (https://jofrhwld.github.io/blog/posts/2025/06/2025-06-16_introducing-tidynorm/). If you do not want to make the switch and would like to use the {{joeyr}} version instead, you still can by calling `joeyr_norm_deltaF()` instead.")
  return(.df)
}



#' Log-means normalization
#'
#' Normalize vowel formant measurements a log-means normalization procedure as
#' described in Barreda & Nearey (2018). This function is intended to be used
#' within a tidyverse pipeline.
#'
#' The data should \emph{not} be grouped beforehand (e.g. with \code{group_by}).
#' The data must be numeric, and there cannot be any \code{NA}s.
#'
#' @param .df The data frame containing the formant measurements you want to
#' normalize. \strong{Formant data must be log transformed!} See example code below.
#' @param .formant_cols The (unquoted) name(s) of the column containing the formant
#' measurements.
#' @param .speaker_col The (unquoted) name of the column containing the unique
#' identifiers per speaker.
#' @param .vowel_col The (unquoted) name of the column containing the unique
#' identifiers per vowel
#' @param .return A string. By default, \code{"data"}, which will returned the
#' your original data with the normalized data appended. If you set this to
#' \code{"params"}, you'll get a data frame with the normalization paramters
#' for each speakers.
#' @param i_know_more_than_you Logical. The function won't work if you've got
#' data that doesn't look like log10-transformed formant data. If you want to
#' force the function to run anyway, set this to `TRUE`.
#'
#' @note As of June 18, 2025, the `norm_logmeans` function is depreciated in favor of
#' the `norm_nearey` function in the tidynorm package. I recommend you switch to
#' that for future code. If you want to retain the current functionality, you can
#' call `joeyr_norm_logmeans` instead.
#'
#' @note
#' Thanks to Santiago Barreda for providing most of the code for this function.
#'
#' @references
#' Barreda, Santiago, and Terrance M. Nearey. 2018. "A Regression Approach to Vowel
#' Normalization for Missing and Unbalanced Data." \emph{The Journal of the Acoustical
#' Society of America} 144(1): 500–520.
#' \href{https://doi.org/10.1121/1.5047742}{https://doi.org/10.1121/1.5047742}.
#'
#' @return The original dataframe with new columns containing the normalized
#' measurements. These new columns have "_norm" appended to the column names.
#'
#' @examples
#' library(tidyverse)
#' idaho <- joeysvowels::idahoans
#'
#' # Basic usage. Note that the data has to be log10-transformed.
#' idaho %>%
#'     mutate(F1_log = log10(F1), F2_log = log10(F2)) %>%
#'     norm_logmeans(.formant_cols = c(F1_log, F2_log),
#'                   .speaker_col = speaker,
#'                   .vowel_col = vowel) %>%
#'     head()
#'
#' # Return the speaker paramters instead.
#' idaho %>%
#'     mutate(F1_log = log10(F1), F2_log = log10(F2)) %>%
#'     norm_logmeans(.formant_cols = c(F1_log, F2_log),
#'                   .speaker_col = speaker,
#'                   .vowel_col = vowel,
#'                   .return = "params") %>%
#'     head()
#'
#' # If you forget to log-transform the data, it'll throw an error.
#' idaho %>%
#'     norm_logmeans(.formant_cols = c(F1, F2),
#'                   .speaker_col = speaker,
#'                   .vowel_col = vowel)
#'
#' # But you can force the function to run on non-transformed data if you're sure you know what you're doing.
#' idaho %>%
#'     norm_logmeans(.formant_cols = c(F1, F2),
#'                   .speaker_col = speaker,
#'                   .vowel_col = vowel,
#'                   i_know_more_than_you = TRUE) %>%
#'     head()
joeyr_norm_logmeans <- function(.df, .formant_cols, .speaker_col, .vowel_col, .return = "data", i_know_more_than_you = FALSE) {

  new_df <- .df %>%
    select({{.formant_cols}}, {{.speaker_col}}, {{.vowel_col}})

  formants <- new_df %>%
    select({{.formant_cols}})

  speakers <- new_df %>%
    pull({{.speaker_col}})

  vowels = new_df %>%
    pull({{.vowel_col}})

  # Quicky check to see if it's log data
  max_formants <- max(formants)
  min_formants <- min(formants)
  if ((min_formants < 0 | max_formants > 10) & missing(i_know_more_than_you)) {
    stop("Are you sure your formant data is log-transformed? If it's not, see ?tidy_norm for code on how to do that. If you are certain you're right, please add `i_know_more_than_you = TRUE` to this function.")
  }


  # The rest of this code is from Santiago

  speakers = factor(speakers)
  vowels = factor(vowels)
  output = cbind(formants, vowels, speakers)

  nffs = ncol(formants)
  n = nrow (formants)
  ffs = c(unlist(formants))

  if (max(formants) > 20){
    ffs = log (ffs)
    output[,1:nffs] = log(output[,1:nffs])
  }
  omit = (ffs!='-Inf')

  fnum = as.factor (rep(1:nffs, each = n))
  speakers = as.factor(rep(speakers,nffs))
  vowels = as.factor(rep(vowels,nffs))
  vk = interaction (vowels, fnum)
  contrasts (vk) = 'contr.sum'
  nspeakers = phonTools::ntypes(speakers)
  nvowels = phonTools::ntypes(vowels)

  model = lm (ffs[omit] ~ 0+speakers[omit] + vk[omit])
  gbars = model$coefficients[1:nspeakers]

  output$gbar = 0
  for (i in 1:nspeakers){
    output[levels(speakers)[i]==output$speakers,1:nffs] = output[levels(speakers)[i]==output$speakers,1:nffs] - gbars[i]
    output$gbar[levels(speakers)[i]==output$speakers] = gbars[i]
  }
  for (i in 1:nffs) output[output[,i]=='-Inf',i] = 0
  # return (output)


  # This is mine again, to get the output how I want it.
  if (.return == "params") {
    output %>%
      select(speakers, gbar) %>%
      distinct() %>%
      return()
  } else {
    output_to_bind <- output %>%
      select(-vowels, -speakers, -gbar) %>%
      rename_all(~paste0(., "_logmeans"))

    .df %>%
      cbind(output_to_bind)
  }

}

#' @rdname joeyr_norm_logmeans
#' @export
norm_logmeans <- function(.df) {
  warning("As of June 18, 2025 (joeyr version 0.10), the `norm_logmeans` function has been depreciated in the {{joeyr}} package. I recommend you switch to the {{tidynorm}} package by Josef Fruehwald. See the package website here (https://jofrhwld.github.io/tidynorm/) and an introduction to the package here (https://jofrhwld.github.io/blog/posts/2025/06/2025-06-16_introducing-tidynorm/). If you do not want to make the switch and would like to use the {{joeyr}} version instead, you still can by calling `joeyr_norm_logmeans()` instead.")
  return(.df)
}
