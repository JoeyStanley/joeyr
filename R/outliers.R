#' Detect outliers
#'
#' This is an implementation of the Mahalanobis Distance that is less sensitive
#' to outliers, first implemented in Stanley (2020). Instead of a blanket filter
#' applying all at once, it iteratively removes points one at a time until a
#' predetermined proportion of data has been removed.
#'
#' The Mahalanobis distance function is somewhat sensitive to outliers, so if
#' there are extreme values in your data, the mean value will be off-center from
#' the centroid of your observations. Consequently, the Mahalanobis Distances
#' will be based on this off-center points, which is probably not desirable.
#' This function alleviates this sensitivity to outliers by implementing a
#' one-at-a-time method.
#'
#' When you run this function, it will first calculate Mahalanobis distance from
#' the mean of all values. It detects the point furthest from the mean and
#' removes it. Then, it recalculates the Mahalanobis distance with the remaining
#' values and again removes the furthest value. It continues this
#' recalculation-and-removal method until a predetermined proportion of values
#' has been removed.
#' @param ... A list of columns in your data that should be included when
#'   calculating the Mahalanobis distance. The column names should not be in
#'   quotes. For vowel data, you typically include F1 and F2. You may also
#'   want to include F3, duration, and any other continuous variable.
#' @param keep A number indicating the proportion of data (per group) to keep.
#'   By default, it's 0.95 so it keeps 95\% of the data and filters out 5\%.
#' @param verbose logical, \code{FALSE} by default. If \code{TRUE}, you'll get
#'   a message for every group with less than 20 tokens saying that there
#'   weren't enough tokens to remove outliers. Can be quite verbose if there are
#'   many speakers/vowels.
#' @return A vector of TRUE/FALSE values. They are in the same order as the original
#'   dataset. Observations that are considered outliers have the value TRUE. It is
#'   easiest to work with this by appending this vector to your dataframe.
#' @note While not required, you should typically "group" your data before applying
#'   this function. For example, you can group your data by speaker and vowel so
#'   that the function applies independently for each vowel for each speaker. I
#'   normally do this with \code{dplyr::group_by(speaker, word)}
#'
#'   Note also that in American English, allophonic variation of some vowels is so
#'   great that grouping by vowel may not be enough. If you're working with /u/ for
#'   example, it's a good idea to split it into three groups: post-coronal, pre-lateral,
#'   and elsewhere. For /æ/, it's a good idea to group prenasal tokens separately.
#'   If you're using FAVE/DARLA/MFA output, the NORTH and FORCE classes of words
#'   are transcribed with AO, so it's a good idea to treat those separately. The point
#'   is to be mindful of allophonic variation in your data and that it's a good
#'   idea to group the data by vowel \emph{class} rather than by vowel. You may have to
#'   do some processing before the filter happens to get this to happen. As of
#'   version 0.8 of joeyr, you can now use the \code{code_allophones} function
#'   to automatically classify your data into allophones.
#'
#'   Finally, be aware that no tokens will be marked as outliers if the are not
#'   a sufficient number of tokens. So if you want to remove 5% of the tokens,
#'   you'll need to have at least 20 tokens in a group for an outlier to be
#'   found within that group. A message will let you know if this happens.
#'   Unfortunately, the function cannot help determine which group(s) the
#'   message came from, but you can find out with \code{dplyr::count()}. See the
#'   examples.
#'
#' @references
#' If you use this function, you can refer to it as something like "the Modified
#' Mahalanobis Distance method implemented in Stanley (2020)."
#'
#' Stanley, Joseph A. "The Absence of a Religiolect among Latter-Day Saints in
#' Southwest Washington." In \emph{Speech in the Western States: Volume 3,
#' Understudied Varieties}, by Valerie Fridland, Alicia Beckford Wassink, Lauren
#' Hall-Lew, and Tyler Kendall, 95–122. Publication of the American Dialect
#' Society 105. Durham, NC: Duke University Press, 2020.
#' \href{https://doi.org/10.1215/00031283-8820642}{https://doi.org/10.1215/00031283-8820642}.
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' df <- joeysvowels::coronals
#'
#' # You can output the data to a column called something like "is_outlier" and
#' # then filter out values that are TRUE.
#' df %>%
#'    group_by(vowel) %>%
#'    mutate(is_outlier = find_outliers(F1, F2, keep = 0.95)) %>%
#'    filter(!is_outlier)
#'
#' # Alternatively, you can skip a step and just keep the data that are not
#' # outliers.
#' df %>%
#'    group_by(vowel) %>%
#'    filter(!find_outliers(F1, F2))
#'
#' # In some cases, you might not have enough data. In this case, a warning
#' # message will appear.
#' df %>%
#'     filter(percent == 50) %>%
#'     group_by(vowel) %>%
#'     mutate(is_outlier = find_outliers(F1, F2, keep = 0.95))
#' # You can find out which groups have less than 20 tokens with `dplyr::count()`:
#' df %>%
#'     filter(percent == 50) %>%
#'     group_by(vowel) %>%
#'     count()
find_outliers <- function(..., keep = 0.95, verbose = FALSE) {

  # Capture that arbitrary list of variables.
  vars <- list(...)
  # Turn it into a dataframe
  df <- as.data.frame(vars, col.names = letters[1:length(vars)])
  # Figure out how many variables will be used.
  n_vars <- length(df)
  # Add a new column
  df$is_outlier <- FALSE
  df$id <- 1:nrow(df)

  # Get the length of the first (or any) of them
  total_n <- length(vars[[1]])
  # Figure out how many to remove
  n_to_remove <- floor(total_n * (1-keep))

  # Don't do anything for small groups
  if (n_to_remove == 0) {

    if (verbose) {
      message(paste("With only", total_n, "tokens, there are not enough tokens to determine outliers."))
    }

  } else {

    # Loop through and remove the points one at a time.
    for (i in 1:n_to_remove) {

      # A new df with just the tokens not marked as outliers
      this_loop_df <- df[df$is_outlier == FALSE,]
      # A subset of that with just the columns that'll go into the mahalanobis() function
      data_for_mahal <- this_loop_df[,1:n_vars]

      # *Tips hat to Joe Fruehwald via Renwick for this line of code.*
      t_params <- MASS::cov.trob(data_for_mahal)
      this_loop_df$mahal_dist <- mahalanobis(data_for_mahal,
                                             center = t_params$center,
                                             cov    = t_params$cov)

      # # Add the mahalanobis distance to this iteration's dataframe. Sort so that the largest distance is first.
      this_loop_df <- this_loop_df[order(this_loop_df$mahal_dist, decreasing = TRUE),]

      # Extract the id of the largest distance.
      id_of_furthest_token <- this_loop_df$id[[1]]

      # Mark that one token as an outlier.
      df[df$id == id_of_furthest_token, "is_outlier"] <- TRUE
    }

  }

  # Return the vector that determines which points are outliers.
  df$is_outlier
}
