# The filter functions --------------------------------------------------------------

#' Detect outliers
#'
#' This is an implementation of the Mahalanobis Distance that is less sensitive
#' to outliers. Instead of a blanket filter applying all at once, it iteratively 
#' removes points one at a time until a predetermined proportion of data has been
#' removed.
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
#'   and elsewhere. For /æ/, it's a good idea to group prenasal tokens separatly. 
#'   If you're using FAVE/DARLA/MFA output, the NORTH and FORCE classes of words
#'   are transcribed with AO, so it's a good idea to treat those separately. The point 
#'   is to be mindful of allophonic variation in your data and that it's a good 
#'   idea to group the data by vowel \emph{class} rather than by vowel. You may have to 
#'   do some processing before the filter happens to get this to happen.
#'   
#' @examples
#' # Load some data and group it by vowel.
#' require(dplyr)
#' joey_vowels <- read.csv("http://joeystanley.com/data/joey.csv") %>%
#'    group_by(vowel)
#'
#' # You can output the data to a column called something like 
#' # "is_outlier" and then filter values that are false.
#' joey_vowels %>%
#'    mutate(is_outlier = find_outliers(F1, F2, dur, keep = 0.95)) %>%
#'    filter(!is_outlier)
#'
#' # Alternatively, you can skip a step and just keep the 
#' # data that are not outliers.
#' joey_vowels %>%
#'    filter(!find_outliers(F1, F2, dur, keep = 0.95))
find_outliers <- function(..., keep = 0.95) {
  
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
  
  # Return the vector that determines which points are outliers.
  df$is_outlier
}





#'Filter vowel measurements.
#'
#'Description goes here.
#'
#'@param df A grouped dataframe including F1, F2, and vowel columns.
#'@param F1 A quoted string of the column name containing F1 measurements. By
#'  default, a column named "F1" is used.
#'@param F2 A quoted string of the column name containing F2 measurements. By
#'  default, a column named "F2" is used.
#'@param vowel A quoted string of the column name containing the "vowel" column.
#'  This would be the column that identifies what vowel quality the observations
#'  in the row belong to. By default, a column named "vowel" is used.
#'@param method A string. By default, \code{"pca"} is used, which will perform a
#'  principal components analysis on the data and do a regression analysis on
#'  the first two principal components. This is prefered since it takes into
#'  account the correlation in the data.
#'
#'  As an alternative, \code{"f1f2"} can be used instead (technically any string
#'  other than \code{"pca"}), which does a regression on the raw formant
#'  measurements. This is akin to filtering by normalized F1 and F2 (perhaps by
#'  removing anything more than 3 standard deviations from the mean), but it
#'  finds a new mean and standard deviation after removing each point.
#'@param filter A string. By default, the method is "cooksd", meaning that
#'  outliers are detected using Cook's D. Any observation with a Cook's D
#'  greater than the \code{"cutoff"} is considered an outlier. If any other
#'  string is used, such as \"z-score\" than outliers are detected based on a
#'  z-score. The data is normalized and any observation further than *n*
#'  standard deviations from the mean is considered an outlier. The cutoff, in
#'  standard deviation units, is specified in the \code{"cutoff"} parameter.
#'@param cutoff A number. When using Cook's D as the \code{"filter"}, any
#'  observation with a Cook's D higher than this value is considered an outlier.
#'  By default, the rule of thumb for Cook's D is used: \deqn{4/(n - k - 1)}
#'  where \emph{n} is the number of observations, and \emph{k} is the number of
#'  variables, which in this case, is 2 (PC1 and PC2 or F1 and F2).
#'
#'  Usually, the number of observations will be different for each vowel within
#'  the same speaker, so this value will be different from vowel to vowel. If,
#'  however, a specific cutoff value is wanted that should be used for all
#'  vowels regardless of how many observations there are or their distribution,
#'  it can be specified here.
#'
#'  When using z-score as the \code{"filter"}, the default is that any
#'  observations further than 2 standard deviations from the mean are considered
#'  outliers. This can changed by specifying a number here.
#'@param boost A number. When using \code{"Cook's D"} as the \code{"method"},
#'  the rule of thumb cutoff value is often too strong and removes too many
#'  observations. To make the filter weaker, the cutoff value is multiplied by
#'  the \code{"boost"} value. By default, it is set to \code{"3"}. To apply no
#'  filter and to use the exact cutoff value, set this value to \code{"1"}. If
#'  the \code{"cutoff"} value is specified or if z-score is used as the filter,
#'  this parameter is quiety ignored.
#'@param keep Logical. By default, outliers are removed from the data frame.
#'  However, if you would like to keep them (for examination), set this argument
#'  to \code{"TRUE"}. This will create a new TRUE/FALSE column called
#'  \code{"is_outlier"} that will identify whether that particular observation
#'  was determined to be an outlier.
#'
#'@return A dataframe. Outliers are removed if \code{"keep = FALSE"} so there
#'  will be fewer rows. If \code{"keep = TRUE"}, a new variable,
#'  \code{"is_outlier"} will be tagged on the end.
#'
#' @examples
#' laterals <- read.csv("joey.csv") %>%
#'     group_by(vowel)
#'
#' # Detect outliers using Cook's D based on a PCA regression.
#' joey_filter(laterals)
#' # a stronger filter
#' joey_filter(laterals, boost = 2)
#' # with blanket cutoff score
#' joey_filter(laterals, cutoff = 0.1)
#'
#' # Detect outliers using Cook's D based on F1 and F2 values.
#' joey_filter(laterals, method = "f1f2")
#'
#' # Detect outliers using z-score based on PCA regression.
#' joey_filter(laterals, filter = "z-score")
#'
#' # Detect outliers using z-score based on F1 and F2 values.
#' joey_filter(laterals, method = "f1f2", filter = "z-score")
#' # Based z > 2.5 instead of z > 2.
#' joey_filter(laterals, method = "f1f2", filter = "z-score", cutoff = 2.5)
#'
#' # Specify column names.
#' joey_filter(laterals, F1 = "F1.50.", F2 = "F2.50.", vowel = "phoneme")
#'
#' # Keep the outliers.
#' joey_filter(laterals, keep = TRUE)

joey_filter <- function(df, F1 = "F1", F2 = "F2", vowel = "vowel",
                        method = "pca", filter = "cooksd", cutoff = 0, boost = 3, keep = FALSE) {
  
    warning("This function is unstable and unfinished and will be removed in a future version of the package. Please use the new-and-improved, `joeyr::find_outliers``, instead.")
    
    # End now if the data isn't a grouped dataframe.
    if (!"grouped_df" %in% class(df)) {
        warning("Just FYI, this function only works on a grouped dataframe. For example:",
                "\n\n\tdf %>%\n\t  dplyr::group_by() %>%\n\t  rm_outliers()",
                "\n\nI plan on this function working on ungrouped data in the future. ",
                "For now I'll just return your original data without making any changes...")
        return(df)
    }
    
    # Get the number of groups (=unique combinations of factors if multiple groups are selected)
    indices <- attributes(df)$indices
    group_combos <- attributes(df)$labels
    
    # Make an empty list the same length as the number of groups.
    # I'd like to make this purrr::map or Xapply, but I can't figure it out for now.
    all_list <- list(1:length(indices))
    for (i in 1:length(indices)) {
        
        # For this iteration, just get those rows that are in this group.
        # For some reason, I need to add 1. So weird.
        this_group <- df[indices[[i]]+1,]
        
        this_combo <- group_combos[i,]
        
        # Pass that subset down into the main function, with all the other arguments.
        all_list[[i]] <- joey_rm_outliers(this_group, this_combo, F1, F2, vowel,
                                          method, filter, cutoff, boost, keep)
    }
    
    # Turns the list into a long dataframe.
    all_df <- all_list %>% dplyr::bind_rows()
    return(all_df)
}

#' Run a Principal Components Analysis
#'
#' This function should only be used as called in \code{"joey_filter"}. It runs
#' a PCA on F1 and F2 of the data frame, saves the PCs, and returns the data
#' frame.
#'
joey_do_pca <- function(still_good) {
    
    # Run the PCA.
    pca <- prcomp( ~ cbind(F1, F2), data = still_good, scale=TRUE, center=TRUE)
    
    # Add the PCA columns to the df
    still_good$PC1 = pca$x[,1]
    still_good$PC2 = pca$x[,2]
    
    return(still_good)
}
#' Adjust the cutoff score.
#'
#' This function should only be used as called in \code{"joey_filter"}. This
#' adjusts the cutoff scores depending on the parameters of the previous
#' function.
#'
joey_adjust_cutoff <- function(still_good, cutoff, boost, filter) {
    
    if (filter == "cooksd") {
        # Rule-of-thumb cutoff is D > 4/(n-k-1)
        # Since we're just working with F1 and F2, k is always 2
        if (cutoff == 0) {
            this_cutoff <- 4/(nrow(still_good)-3) * boost
        } else {
            this_cutoff <- cutoff
        }
        
        # This includes Mahalanobis and F1F2
    } else {
        
        # If no cutoff is defined, use the default.
        if (cutoff == 0) {
            # We'll use 2 standard deviations as the default.
            this_cutoff <- 2
        } else {
            this_cutoff <- cutoff
        }
    }
    
    return(this_cutoff)
}


#' Remove outliers
#'
#' This function should only be used as called in \code{"joey_filter"}. This is actually the function that does all the work and contains the loop but you can only use it by calling the \code{"joey_filter"} function.
#'
joey_rm_outliers <- function(df, this_combo, F1, F2, vowel, method, filter, cutoff, boost, keep) {
    # To do: make this message system work.
    # If one group is used, it's a string.
    # If two groups are used, it's a 1x2 data frame.
    message("Now working on ", this_combo)
    
    if (nrow(df) < 10) {
        warning("At least 10 observations are needed. None were removed from ", this_combo)
    }
    
    # For now, just assume we filter along F1 and F2 only.
    # A fixed number of variants are possible (such as F3 and dur).
    # Eventually, use the ... like so: variables <- list(...)
    # This page shows how to do this without quotes
    # https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function
    df$F1 <- df[[F1]]
    df$F2 <- df[[F2]]
    df$vowel <- df[[vowel]]
    
    # Remove the NAs from just F1 and F2.
    #df <- df[complete.cases(F1, F2),] # <- this is not working the way I want it to. Had to resort to tidyverse.
    df <- df %>% tidyr::drop_na(F1, F2)
    
    # Assume everything is good from the start.
    df$is_outlier <- FALSE
    
    # Start the loop.
    while(TRUE) {
        
        # For this loop, work with only the ones that are not marked as "is_outlier" still.
        still_good <- subset(df, is_outlier==FALSE)
        
        # Check to make sure there's enough data in each vowel.
        if (nrow(still_good) <= 10) {
            warning(paste0("The filter is too strong and has removed pretty much all the points from ",
                           unique(df$vowel), "."))
            break
        }
        
        # Get the cutoff value and adjust with the boost if necessary.
        # This should run every time. It doesn't need to for sd, but it does for Cook's D.
        this_cutoff <- joey_adjust_cutoff(still_good, cutoff, boost, filter)
        
        # If we're doing PCA
        if (method == "pca") {
            
            # Do the PCA and save the PCs.
            still_good <- joey_do_pca(still_good)
            
            # Save the Cooks D.
            if (filter == "cooksd") {
                # Save the Cook's D on the PCA data
                still_good$d1 <- cooks.distance(lm(PC1~PC2, data=still_good))
                still_good$d2 <- cooks.distance(lm(PC2~PC1, data=still_good))
                
            } else {
                # Save the normalized values based on the PCs.
                still_good$d1 <- (still_good$PC1 - mean(still_good$PC1)) / sd(still_good$PC1)
                still_good$d2 <- (still_good$PC2 - mean(still_good$PC2)) / sd(still_good$PC2)
            }
            
            # Remove the PC columns now that we're done with it.
            still_good <- still_good %>% select(-PC1, -PC2)
            
        } else if (method == "mahalanobis") {
            
            # Save just the relevant columns
            justF1F2 <- cbind(still_good$F1, still_good$F2)
            
            # Save the Mahalanobis Distance (twice to not break later code)
            still_good$d1 <- mahalanobis(justF1F2, colMeans(justF1F2), cov(justF1F2))
            still_good$d2 <- still_good$d1
            
            # Instead, if PCA is not used, use just F1 and F2 instead.
        } else {
            
            if (filter == "cooksd") {
                # Save the Cook's D on the raw F1 and F2 values.
                still_good$d1 <- cooks.distance(lm(F1~F2,  data=still_good))
                still_good$d2 <- cooks.distance(lm(F1~F2,  data=still_good))
                
            } else {
                # Save the normalized values based on the original F1 and F2 values.
                still_good$d1 <- (still_good$F1 - mean(still_good$F1)) / sd(still_good$F1)
                still_good$d2 <- (still_good$F2 - mean(still_good$F2)) / sd(still_good$F2)
            }
        }
        
        # Sort by D and just keep the highest one
        worst1 <- still_good[still_good$d1 == max(still_good$d1),]
        worst2 <- still_good[still_good$d2 == max(still_good$d2),]
        
        #message(worst1)
        
        # If everything has a low Cook's D, we're good. Exit the loop.
        if (worst1$d1 <= this_cutoff & worst2$d2 <= this_cutoff) {
            break
        }
        
        # Remove bad F1.
        if (worst1$d1 > this_cutoff) {
            # Look through the original and find which one matches this round's max
            # This is assuming no two values have the same F1 and F2...
            # Crucially, make these changes to "df", not "still_good".
            df[df$F1 == worst1$F1 & df$F2 == worst1$F2,]$is_outlier <- TRUE
        }
        
        # Remove bad F2.
        if (worst2$d2 > this_cutoff) {
            df[df$F1==worst2$F1 & df$F2==worst2$F2,]$is_outlier <- TRUE
        }
    }
    
    # We're now done with the loop. All the bad ones are marked with "is_outlier" as TRUE.
    
    # Filter out the bad ones now
    if (keep == FALSE) {
        df <- df %>%
            dplyr::filter(is_outlier==FALSE) %>%
            dplyr::select(-is_outlier)
    }
    
    return(df)
}
