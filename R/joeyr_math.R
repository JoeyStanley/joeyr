
#' Barks and Hertz
#'
#' Convert Hz into Barks and vice versa. The formula and inverse are taken from Traunmüller (1990).
#'
#' @param x A number
#' @return The converted number.
#' @examples
#' bark(1000) # 8.527432
#' hz(8.527432) # 1000

# Convert Hz to Barks (more common)
bark <- function(f) {
    
    # Formula give in NORM
    # 26.81 / (1 + 1960 / f) - 0.53
    
    # Formula found in the actual reference
    (26.81 * f / (1960 + f)) - 0.53
}

#' @rdname bark
hz <- function(z) {
    1960 * (z + 0.53) / (26.28 - z)
}



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
#' @param ... Names of columns that should be included in the Mahalnobis distance. For vowel data, this is typically your F1 and F2 columns.
#' @return A vector that contains the Mahalabis distances for each observation.
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

