
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
#' within a tidyverse pipeline, though the function itself uses only base R. 
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












#' Code allophones
#' 
#' A function to classify vowel data into contextual allophones. 
#'  
#' @param .df The dataset containing vowel data.
#' @param .old_col The unquoted name of the column containing the vowel labels. 
#' Often called "vowel" or "phoneme" in many datasets. Note that the function 
#' assumes Wells lexical sets (FLEECE, TRAP, etc.) rather than ARPABET (IY, AE, etc.) 
#' or IPA (i, æ, etc.). If your vowels are not already coded using Wells' labels
#' you can quickly do so with \code{switch_transcriptions} or one of the 
#' shortcuts like \code{arpa_to_wells}
#' @param .new_cols A vector of two strings containing the names of the columns
#' you would like to use. By default \code{c("allophone", "allophone_environment")}.
#' The first name becomes the name of the column containing the new allophone
#' labels. The second column becomes the name of the column describing those
#' labels.
#' @param .pre_seg The unquoted name of the column that contains the labels for 
#' the previous segement. In DARLA-generated spreadsheets, this is `pre_seg` and 
#' in FastTrack-generated spreadsheets, it's `previous_sound`. Assumes ARPABET 
#' labels.
#' @param .fol_seg The unquoted name of the column that contains the labels for 
#' the following segement. In DARLA-generated spreadsheets, this is `fol_seg` and 
#' in FastTrack-generated spreadsheets, it's `next_sound`. Assumes ARPABET labels.
#' @param .coronals A vector of strings containing ARPABET labels for coronal consonants.
#' By default, \code{c("T", "D", "S", "Z", "SH", "ZH", "JH", "N")}. This is used
#' to create the `TOOT` allophone of `GOOSE`. 
#' @param .voiceless  A vector of strings containing ARPABET labels for voiceless
#' consonants. By default, \code{c("P", "T", "K", "CH", "F", "TH", "S", "SH")}. 
#' This is used to create the `PRICE` allophone of `PRIZE`. 
#' 
#' @note Here are the list of the contextual allophones that are created. Note 
#' that I largely follow my own advice about what to call \href{https://joeystanley.com/blog/why-do-people-use-bat-instead-of-trap}{elsewhere allophones},
#' what to call \href{https://joeystanley.com/blog/extending-wells-lexical-sets-to-prelateral-vowels}{prelateral allophones}, 
#' and \href{https://joeystanley.com/blog/thoughts-on-allophonic-extensions-to-wells-lexical-sets}{other allophones}. 
#' Obviously, this list is pretty subjective and largely based on what my own 
#' research has needed, so it may not work completely for you and your research. 
#' Please contact me at \email{joey_stanley@byu.edu} if you want to see an allophone 
#' get added or if you spot an error in the coding.
#' 
#' \itemize{
#'   \item FLEECE becomes 
#'     \itemize{
#'       \item ZEAL before laterals
#'       \item BEET elsewhere
#'     }
#'   \item KIT becomes \itemize{
#'     \item GUILT before laterals
#'     \item NEAR before rhotics
#'     \item BIG before G
#'     \item BIN before M and N
#'     \item BING before NG
#'     \item BIT elsewhere
#'     }
#'   \item FACE becomes \itemize{
#'     \item FLAIL before laterals
#'     \item VAGUE before G
#'     \item X elsewhere
#'     }
#'   \item DRESS becomes \itemize{
#'     \item SHELF before laterals
#'     \item SQUARE before rhotics
#'     \item BEG before G
#'     \item BEN before M and N
#'     \item BENG before NG
#'     \item BET elsewhere
#'     }
#'   \item TRAP becomes \itemize{
#'     \item TALC before laterals
#'     \item BAG before G
#'     \item BAN before M and N
#'     \item BANG before NG
#'     \item BAT elsewhere
#'     }
#'   \item LOT becomes \itemize{
#'     \item GOLF before laterals
#'     \item START before rhotics
#'     \item BOT elsewhere
#'     }
#'   \item THOUGHT becomes \itemize{
#'     \item FAULT before laterals
#'     \item FORCE befpre rhotics
#'     \item BOUGHT elsewhere
#'     }
#'   \item STRUT becomes \itemize{
#'     \item MULCH before laterals
#'     \item BUT elsewhere
#'     }
#'   \item GOAT becomes \itemize{
#'     \item JOLT before laterals
#'     \item BOAT elsewhere
#'     }
#'   \item FOOT becomes \itemize{
#'     \item WOLF before laterals
#'     \item CURE before rhotics
#'     \item PUT elsewhere
#'     }
#'   \item GOOSE becomes \itemize{
#'     \item MULE before Y
#'     \item TOOT before coronals
#'     \item SPOOL before laterals
#'     \item BOOT elsewhere
#'     }
#'   \item PRICE becomes \itemize{
#'     \item PRICE before voiceless segments
#'     \item PRIZE elsewhere
#'     }
#'   }
#'   
#' Unfortunately, it is not straightforward to customize this list but you can 
#' always copy the source code and modify the list yourself. 
#' 
#' Alternatively, you can use \code{forcats::fct_collapse()} to collapse 
#' distinctions that you don't need. See example code below. 
#' 
#' You can also of course create your own allophones if desired. Note that some
#' allophones depend on other environmental information like syllable structure and  
#' morpheme/word boundaries, or they may be entirely lexical (FORCE vs. NORTH). 
#' They may be more complicated than what ARPABET can code for (MARY, MERRY, and
#' MARRY) or just inconsistently coded. For the sake of simplicity, these 
#' allophones are not included in this function.
#' 
#' The environments therefore are the following 
#' \itemize{
#'   \item "prelateral" includes ZEAL, GUILT, FLAIL, SHELF, TALC, GOLF, FAULT, MULCH, JOLT, WOLF, SPOOL
#'   \item "prerhotic" includes NEAR, SQUARE, START, FORCE, CURE
#'   \item "prevelar" includes BIG, VAGUE, BEG, BAG, 
#'   \item "prenasal" includes BIN, BEN, BAN
#'   \item "prevelarnasal" includes BING, BENG, BANG
#'   \item "prevoiceless" includes PRICE
#'   \item "post-Y" includes MULE
#'   \item "postcoronal" includes TOOT
#'   \item "elsewhere" includes BEET, BIT, BAIT, BET, BAT, BOT, BOUGHT, BUT, BOAT, PUT, BOOT, PRIZE
#' }
#' 
#' 
#' 
#' @return A dataframe with two additional columns. One column contains labels 
#' for the allophones and the other contains category labels for those 
#' allophones' contexts. The second column can be useful for quickly excluding 
#' certain allophones like prelaterals or prerhotics or coloring families of 
#' allophones in visualizations (such as turning all prelateral allophones gray). 
#' These two new columns are positioned immediately after the original vowel 
#' column indicated in \code{.old_col},
#' 
#' @examples
#' suppressPackageStartupMessages(library(tidyverse))
#' 
#' # Get some sample DARLA data to play with
#' darla <- joeysvowels::darla %>%
#'   select(word, vowel, pre_seg, fol_seg) %>%
#'   mutate(phoneme = joeyr:::arpa_to_wells(vowel), .after = vowel)
#' 
#' # Basic usage
#' darla %>%
#'   code_allophones(.old_col = phoneme, .fol_seg = fol_seg, .pre_seg = pre_seg) %>%
#'   slice_sample(n = 20)
#' 
#' # Specify the names of the new columns with the `.new_cols` argument
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"), 
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg) %>%
#'   slice_sample(n = 20)
#' 
#' # Filtering by environment is straightforward
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"), 
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg) %>%
#'   filter(environment == "elsewhere") %>%
#'   slice_sample(n = 20)
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"), 
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg) %>%
#'   filter(!environment %in% c("prerhotic", "prevelarnasal", "prevelar")) %>%
#'   slice_sample(n = 20)
#' 
#' # Some users may want to supply their own list of coronal consonants.
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"),
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg,
#'                   .coronals = c("T", "D", "S", "Z", "SH", "ZH", "JH", "N", "Y")) %>%
#'   filter(phoneme == "GOOSE") %>%
#'   slice_sample(n = 20)
#' 
#' # Other users may want to specify their own list of voiceless consonants. 
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"),
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg,
#'                   .voiceless = c("P", "T", "K", "CH", "F", "TH", "S", "SH", "X")) %>%
#'   filter(phoneme == "PRICE") %>%
#'   slice_sample(n = 20)
#' 
#' # Collapsing distinctions can be done post hoc (though it may take extra work to get the environment column to match.)
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"), 
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg) %>%
#'   # Get a subset for demonstration purposes
#'   filter(allophone %in% c("BIT", "BIG")) %>%
#'   group_by(allophone) %>%
#'   slice_sample(n = 5) %>%
#'   ungroup() %>%
#'   # Now collapse distinctions
#'   mutate(allophone = fct_collapse(allophone, "BIT" = c("BIT", "BIG")),
#'          environment = ifelse(allophone == "BIT", "elsewhere", allophone))
#' 
#' # Creating new allophones depends on the complexity of the allophone         
#' darla %>%
#'   code_allophones(.old_col = phoneme, 
#'                   .new_cols = c("allophone", "environment"), 
#'                   .fol_seg = fol_seg, 
#'                   .pre_seg = pre_seg) %>%
#'   # Create voice and voiceless distinctions for MOUTH
#'   mutate(allophone = case_when(phoneme == "MOUTH" & fol_seg %in% c("P", "T", "K", "CH", "F", "TH", "S", "SH") ~ "BOUT",
#'                                phoneme == "MOUTH" ~ "LOUD",
#'                                TRUE ~ allophone),
#'          environment = if_else(allophone == "BOUT",  "prevoiceless", environment)) %>%
#'   # Get a subset for demonstration purposes
#'   filter(phoneme == "MOUTH") %>%
#'   group_by(allophone) %>%
#'   slice_sample(n = 5) %>%
#'   ungroup()
code_allophones <- function(.df, .old_col, .new_cols = c("allophone", "allophone_environment"), 
                            .pre_seg, .fol_seg,
                            .coronals = c("T", "D", "S", "Z", "SH", "ZH", "JH", "N"),
                            .voiceless = c("P", "T", "K", "CH", "F", "TH", "S", "SH")) {
  
  new_allophone_colname = .new_cols[[1]]
  new_environment_colname = .new_cols[[2]]
  
  .df %>%
    mutate({{new_allophone_colname}} := case_when(
      {{.old_col}} == "FLEECE" ~ 
        case_when({{.fol_seg}} == "L" ~ "ZEAL",
                  TRUE ~ "BEET"),
      {{.old_col}} == "KIT" ~
        case_when({{.fol_seg}} == "L" ~ "GUILT",
                  {{.fol_seg}} == "R" ~ "NEAR",
                  {{.fol_seg}} == "G" ~ "BIG",
                  {{.fol_seg}} %in% c("M", "N") ~ "BIN",
                  {{.fol_seg}} == "NG" ~ "BING",
                  TRUE ~ "BIT"),
      {{.old_col}} == "FACE" ~ 
        case_when({{.fol_seg}} == "L" ~ "FLAIL",
                  {{.fol_seg}} == "G" ~ "VAGUE",
                  TRUE ~ "BAIT"),
      {{.old_col}} == "DRESS" ~
        case_when({{.fol_seg}} == "L" ~ "SHELF",
                  {{.fol_seg}} == "R" ~ "SQUARE",
                  {{.fol_seg}} == "G" ~ "BEG",
                  {{.fol_seg}} %in% c("M", "N") ~ "BEN",
                  {{.fol_seg}} == "NG" ~ "BENG",
                  TRUE ~ "BET"),
      {{.old_col}} == "TRAP" ~
        case_when({{.fol_seg}} == "L" ~ "TALC",
                  {{.fol_seg}} == "G" ~ "BAG",
                  {{.fol_seg}} %in% c("M", "N") ~ "BAN",
                  {{.fol_seg}} == "NG" ~ "BANG",
                  TRUE ~ "BAT"),
      {{.old_col}} == "LOT" ~ 
        case_when({{.fol_seg}} == "L" ~ "GOLF",
                  {{.fol_seg}} == "R" ~ "START",
                  TRUE ~ "BOT"),
      {{.old_col}} == "THOUGHT" ~ 
        case_when({{.fol_seg}} == "L" ~ "FAULT",
                  {{.fol_seg}} == "R" ~ "FORCE",
                  TRUE ~ "BOUGHT"),
      {{.old_col}} == "STRUT" ~
        case_when({{.fol_seg}} == "L" ~ "MULCH",
                  TRUE ~ "BUT"),
      {{.old_col}} == "GOAT" ~
        case_when({{.fol_seg}} == "L" ~ "JOLT",
                  TRUE ~ "BOAT"),
      {{.old_col}} == "FOOT" ~
        case_when({{.fol_seg}} == "L" ~ "WOLF",
                  {{.fol_seg}} == "R" ~ "CURE",
                  TRUE ~ "PUT"),
      {{.old_col}} == "GOOSE" ~
        case_when({{.pre_seg}} == "Y" ~ "MULE",
                  {{.pre_seg}} %in% .coronals ~ "TOOT",
                  {{.fol_seg}} == "L" ~ "SPOOL",
                  TRUE ~ "BOOT"),
      {{.old_col}} == "PRICE" ~ 
        case_when({{.fol_seg}} %in% .voiceless ~ "PRICE",
                  TRUE ~ "PRIZE"),
      TRUE ~ as.character({{.old_col}})),
      .after = {{.old_col}}) %>%
    
    # Use the .data object since {{}} doesn't work in case_when: https://rlang.r-lib.org/reference/topic-data-mask-programming.html#names-patterns
    mutate({{new_environment_colname}} := case_when(
      .data[[new_allophone_colname]] %in% c("ZEAL", "GUILT", "FLAIL", "SHELF", "TALC", "GOLF", "FAULT", "MULCH", "JOLT", "WOLF", "SPOOL") ~ "prelateral",
      .data[[new_allophone_colname]] %in% c("NEAR", "SQUARE", "START", "FORCE", "CURE") ~ "prerhotic",
      .data[[new_allophone_colname]] %in% c("BIG", "VAGUE", "BEG", "BAG") ~ "prevelar",
      .data[[new_allophone_colname]] %in% c("BIN", "BEN", "BAN") ~ "prenasal",
      .data[[new_allophone_colname]] %in% c("BING", "BENG", "BANG") ~ "prevelarnasal",
      .data[[new_allophone_colname]] %in% c("PRICE") ~ "prevoiceless",
      .data[[new_allophone_colname]] %in% c("MULE") ~ "post-Y",
      .data[[new_allophone_colname]] %in% c("TOOT") ~ "postcoronal",
      .data[[new_allophone_colname]] %in% c("BEET", "BIT", "BAIT", "BET", "BAT", "BOT", "BOUGHT", "BUT", "BOAT", "PUT", "BOOT", "PRIZE", "MOUTH", "CHOICE", "NURSE") ~ "elsewhere",
      TRUE ~ "other"),
      .after = {{new_allophone_colname}})
}
