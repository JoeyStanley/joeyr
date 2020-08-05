#' Color Gradienter
#'
#' Get some number of equidistant colors between two colors.
#'
#' This function takes two colors and a number and returns an n-length list of 
#' colors that are all equidistant from one another. This is handy for plotting 
#' categorical, ordinal, or binned data but you want to use continuous colors.
#' 
#' Note, this is a little buggy and may not work as expected. I don't know 
#' enough about how colors are handled to fix bugs. Please ese with caution.
#'
#' @param hi A string, in RGB format ("#123456")
#' @param lo A string, in RBG format ("#ffffff"). The default is white.
#' @param shades An integer
#' @return A list of strings, in RGB format, the length of `shades` and that are equidistant from each other from `hi` to `lo`.
#' @examples
#' # By itself, it returns just a list.
#' color_gradienter("#33458F", "#ffffff", 5)
#'
#' # This can be fed into ggplot::scale_color_manual()
#' df <- data.frame(x = rnorm(50, 0, 1),
#'                  y = rnorm(50, 0, 1),
#'                  color = sample(letters[1:5], 50, replace = TRUE))
#' ggplot2::ggplot(df, ggplot2::aes(x, y, color = color)) +
#'     ggplot2::geom_point(size = 5) +
#'     ggplot2::scale_color_manual(values = color_gradienter(hi = "#33458F", lo = "#ffffff", 5))

# I wrote this for the GSV, but I think it's a useful tool to have.
# Currently uses some tidyverse functions but it'd be easy to switch over to base R.

color_gradienter <- function(hi, lo = "#ffffff", shades) {
  
  if (stringr::str_detect(hi, "#[0-9a-fA-F]{6}") == FALSE) {
    warning("`hi` must be in RGB format: #123AbC")
  }
  if (stringr::str_detect(lo, "#[0-9a-fA-F]{6}") == FALSE) {
    warning("`lo` must be in RGB format: #123AbC")
  }
  if (shades < 2) {
    warning("The number of shades has to be 2 or more.")
  }
  if (shades %% 1 != 0) {
    warning("The number of shades has to be an integer.")
  }
  
  intervals <- shades - 1
  
  # Separate colors into their RGB values.
  # Convert hexidecimal to integer
  hi_r <- strtoi(substr(hi, 2, 3), base = 16)
  hi_g <- strtoi(substr(hi, 4, 5), base = 16)
  hi_b <- strtoi(substr(hi, 6, 7), base = 16)
  
  lo_r <- strtoi(substr(lo, 2, 3), base = 16)
  lo_g <- strtoi(substr(lo, 4, 5), base = 16)
  lo_b <- strtoi(substr(lo, 6, 7), base = 16)
  
  df <- data.frame(shade_num = 0:intervals)
  df <- dplyr::mutate(df, 
               r = ifelse(lo_r <= hi_r, lo_r, hi_r) + abs(hi_r - lo_r) / intervals * shade_num,
               g = ifelse(lo_g <= hi_g, lo_g, hi_g) + abs(hi_g - lo_g) / intervals * shade_num,
               b = ifelse(lo_b <= hi_b, lo_b, hi_b) + abs(hi_b - lo_b) / intervals * shade_num, 
               dplyr::across(c(r, g, b), round),
               dplyr::across(c(r, g, b), as.hexmode),
               rgb = paste0("#", r, g, b))
  df$rgb
}
color_gradienter("#bbbbbb", shades = 6)


#' Capitalize the first letter
#'
#' Capitalizes the first character is a string. Not particularly robust. Copied over from Perl code.
#'
#' @param str A string.
#' @return The same string with the first letter capitalized.
#' @examples
#' ucfirst("foo bar") # "Foo bar"

# Convert Hz to Barks (more common)
ucfirst <- function(str) {
  paste0(toupper(substr(str, 1, 1)), substr(str, 2, nchar(str)))
}