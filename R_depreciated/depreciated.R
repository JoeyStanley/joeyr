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




#' spread() on multiple values.
#'
#' A modification of tidyr::spread that allows you to use it with multiple `value` columns. 
#' 
#' All credit for this function goes to Dan Sullivan on the RStudio Forums who answered my quesiton \href{https://community.rstudio.com/t/spread-with-multiple-value-columns/5378/2}{here}. 
#' Apparently Kieran Healey seemed to like it and wrote a blog post about the function \href{https://kieranhealy.org/blog/archives/2018/11/06/spreading-multiple-values/}{here}
#' and does a really good job at explaning when you might need to use a function like this.
#' A while later, when Hadley Wickham asked for \href{https://community.rstudio.com/t/interesting-tidy-eval-use-cases/21121/5}{interesting cases where tidy eval is used},
#' I commented with this function. He replied with a shorter version, which is what is implemented now.
#' 
#' I still don't fully understand the code, but it works exactly how I want it to so I keep using it.
#' 
#' @param df The dataframe you want to modify.
#' @param key The column you want to have spread across multiple columns. For tall vowel data, this is usually the "formant" column.
#' @param ... The data you want to have fill those new columns.
#' @examples
#' require(tidyr)
#' 
#' # Example 1
#' df1 <- data.frame(word = rep(c("dog", "cat", "fish"), 2),
#'                   formant = rep(c("F1", "F2"), each = 3),
#'                   hz = c(490, 500, 510, 1590, 1600, 1610),
#'                   bark = bark(c(490, 500, 510, 1590, 1600, 1610)))
#' # Now spread them with each formant and type of measurement in its own column.
#' df1 %>%
#'   spread_n(formant, c(hz, bark))
#' 
#' # Example 2
#' df2 <- data.frame(speaker = rep(c("Amy", "Bob"), each = 3),
#'                   word = rep(c("dog", "cat", "fish"), 2),
#'                   F1 = c(490, 500, 510, 390, 400, 410),
#'                   F2 = c(1590, 1600, 1610, 1390, 1400, 1410))
#' # Change it to one row per speaker
#' df2 %>%
#'   spread_n(word, c(F1, F2))
#' # Change it to one row per word
#' df2 %>%
#'   spread_n(speaker, c(F1, F2))
spread_n <- function(df, key, ...) {
  key <- rlang::enquo(key)
  
  df %>% 
    gather("variable", "value", ...) %>%
    unite(temp, !!key, variable) %>%
    spread(temp, value)
}



#' Move one column after another
#'
#' This quite simply moves one column after another in a data frame. Just a simple 
#' way to rearrange a couple columns without having to use `select`. It saves a bit 
#' of typing and makes it more clear what it is that's happening.
#' 
#' There is a known bug when one of the columns is the first column in the dataset.
#'
#' @param df A data frame.
#' @param x A column in that dataframe.
#' @param y Another column in that dataframe.
#' @return A dataframe, with `x` immediately after/before `y`.
#' @examples
#' sample_df <- data.frame(first = 1:3, middle = 4:6, last = 7:9)
#' move_x_after_y(sample_df, last, first)
move_x_after_y <- function(df, x, y) {
  # Tidyeval, so that the arguments don't have to be quoted.
  x_expr <- enquo(x)
  y_expr <- enquo(y)
  
  # Get the name of the first column. Used in select() below.
  name_of_col1 <- colnames(df)[1]
  col1_expr <- quo(name_of_col1)
  
  df %>%
    # Select column 1 through the reference point.
    select(!! col1_expr : !! y_expr,
           # Then add the one you want to move.
           !! x_expr,
           # Then add everything else.
           everything())
}

#' @rdname move_x_after_y
move_x_before_y <- function(df, x, y) {
  # Tidyeval, so that the arguments don't have to be quoted.
  x_expr <- enquo(x)
  y_expr <- enquo(y)
  y_quo  <- quo_name(y_expr)
  
  # Get the name of the first column. Used in select() below.
  name_of_first_col <- colnames(df)[1]
  first_col <- quo(name_of_first_col)
  
  # To insert column x before y, I need to know what is currently before column y.
  num_of_y_col <- which(colnames(df) == y_quo)
  # To get this column number subtract 1 from col y's number, unless col y *is* the first column.
  num_of_prev_col <- num_of_y_col - ifelse(num_of_y_col == 1, 0, 1)
  # Get the name of the previous column and tidyeval it.
  name_of_prev_col <- colnames(df)[num_of_prev_col]
  prev_col <- quo(name_of_prev_col)
  
  df %>%
    # Select column 1 through the reference point.
    select(!! first_col : !! prev_col,
           # Then add the one you want to move.
           !! x_expr,
           # Then add the one you want to move it before.
           !! y_expr,
           # Then add everything else.
           everything())
}


