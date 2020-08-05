#' The "Not In" function
#'
#' This returns \code{TRUE} if values are \emph{not} in a list. It's the opposite of \code{\%in\%}.
#' This is particularly good when subsetting and you only want to remove a
#' few things (like diphthongs). You can remove those rather than specifying
#' all monophthongs you want to keep.
#'
#' Note that what this function does can easily be accomplished with \code{!x \%in\% y},
#' but I like the "not in" aspect. Probably a holdover from my Perl days...
#'
#' You can also define this function as \code{`\%ni\%` <- Negate(`\%in\%`)} but when looking
#' through the documentation on \%in\%, I found that it might be better to
#' modify the formal definition there instead.
#'
#' Credit to \href{https://stackoverflow.com/questions/24660864/declaring-special-infix-functions-in-r-packages}{this Stack Overflow question} for showing how to get this to work in an R package.
#' @param x A vector, presumably a subset of "y"
#' @param y A vector
#' @return A list of logicals indicating whether the objects in `x` are not contained in `y`.`
#' @examples
#' 1:10 %in% c(1,3,5,9)
#' 1:10 %ni% c(1,3,5,9)
`%ni%` <- function(x, y) {
  match(x, y, nomatch = 0) == 0
}






#' Within
#'
#' Here's a very handy "within" range. Checks whether a number is within a range. Capable of handling 
#' lists so that it can apply to a whole column. Basically I got tired of running a two-part conditional
#' when I wanted something that was within a range: \code{if (x > 2 & x < 5)}.
#'
#' @param x A numeric vector.
#' @param range A numeric vector. The highest and lowest values will be used as the range.
#' @return A logical vector.
#' @examples
#' c(1:5) %wi% c(2,3)
`%wi%` <- function(x, range) {
  
  return_values <- rep(FALSE, length(x))
  for (i in 1:length(x)) {
    if (min(range) <= x[[i]] & x[i] <= max(range)) {
      return_values[i] <- TRUE
    }
  }
  return_values
}


#' Expand a range
#'
#' This function expands ranges by some value. Used in my dissertation code for axis ranges.
#'
#' @param range A numeric vector. The highest and lowest values will be used as the range.
#' @param expansion A number. This number will be added to the highest and subtracted from the lowest values of the range
#' @return A list of length 2
#' @examples
#' c(1,5) %expanded_by% 1
`%expanded_by%` <- function(range, expansion) {
  c(min(range) - expansion,
    max(range) + expansion)
}

