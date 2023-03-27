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
#' library(ggplot2)
#' # By itself, it returns just a list.
#' color_gradienter("#33458F", "#ffffff", 5)
#'
#' # This can be fed into ggplot::scale_color_manual()
#' df <- data.frame(x = rnorm(50, 0, 1),
#'                  y = rnorm(50, 0, 1),
#'                  color = sample(letters[1:5], 50, replace = TRUE))
#' ggplot(df, aes(x, y, color = color)) +
#'     geom_point(size = 5) +
#'     scale_color_manual(values = color_gradienter(hi = "#33458F", lo = "#ffffff", 5))

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
#' @param str a string.
#' @return The same string with the first letter capitalized.
#' @examples
#' ucfirst("foo bar")
ucfirst <- function(str) {
  paste0(toupper(substr(str, 1, 1)), substr(str, 2, nchar(str)))
}




#' Get centroids
#'
#' Runs a summarizing function for each specified column, for each specified group.
#' This is intended to be used to plot centroids in ellipses
#' in ggplot2 without having to create a new object or have a lot of in-line
#' code. See examples below.
#'
#' @param df a dataframe.
#' @param .cols columns that should be summarized. For sociophonetic data, this
#' is usually the names of your vowel columns, e.g. \code{c(F1, F2)}. This literally
#' is just passed into an `across` function within `summarize`.
#' @param ... grouping variables. For sociophonetic data, this might be speaker
#' and allophone or something. This is just passed into `group_by`.
#' @param .fns one or more names of functions. By default, \code{median}. This is
#' passed into `across`.
#'
#' @return an ungrouped dataframe
#'
#' @note Okay technically this function name is a misnomer because we're not
#' truly getting centroids in a mathematical sense. But that's what I think of
#' when I run this so that's what we're going with.
#'
#' @examples
#' library(tidyverse)
#' df <- joeysvowels::idahoans
#'
#' # Basic usage as a summarizing function
#' df %>%
#'   get_centroids(c(F1, F2), vowel)
#'
#' # Within a ggplot2 block. Note that you do have to start the data argument with the dot and pipe it into get_centroids, rather than incorporating it in (i.e. get_centroids(., vowel)). Not sure why but this appears to be a contraint imposed by ggplot2.
#' ggplot(df, aes(F2, F1, color = vowel)) +
#'   stat_ellipse(level = 0.67) +
#'   geom_text(data = . %>% get_centroids(c(F1, F2), vowel), aes(label = vowel)) +
#'   scale_x_reverse() +
#'   scale_y_reverse() +
#'   theme(legend.position = "none")
#'
#' # You can add multiple groups to the code too.
#' ggplot(df, aes(F2, F1, color = vowel)) +
#'   stat_ellipse(level = 0.67) +
#'   geom_text(data = . %>% get_centroids(c(F1, F2), speaker, vowel), aes(label = vowel)) +
#'   scale_x_reverse() +
#'   scale_y_reverse() +
#'   facet_wrap(~speaker, scales = "free") +
#'   theme(legend.position = "none")
#'
#' # Like any use of group_by(), additional, perhaps redundant columns may be specified for the purpose of "passing them through." In this example, adding tense_lax doesn't change the calculations, but it's useful for this plot. Additionally, this block of code highlights one strength of get_centroids, and that is that I can pass in a modified dataframe directly to ggplot and then modify it even further to get the labels, without needing to create any new objects.
#' df %>%
#'   mutate(tense_lax = fct_collapse(vowel,
#'                                   "tense" = c("IY", "EY", "AO", "OW", "UW"),
#'                                   "lax"   = c("IH", "EH", "AE", "AA", "AH", "UH"))) %>%
#'   ggplot(aes(F2, F1, color = tense_lax, group = vowel)) +
#'   stat_ellipse(level = 0.67) +
#'   geom_text(data = . %>% get_centroids(c(F1, F2), speaker, tense_lax, vowel),
#'             aes(label = vowel)) +
#'   scale_x_reverse() +
#'   scale_y_reverse() +
#'   facet_wrap(~speaker, scales = "free") +
#'   theme(legend.position = "none")
#'
#' # For column selection, any tidyselect output works, such as matches().
#' df %>%
#'   get_centroids(matches("F\\d"), speaker, vowel)
#'
#' # For functions, you can add more than one. Just wrap them up into c().
#' df %>%
#'   get_centroids(c(F1, F2), .fns = c(median, mean), speaker, vowel)
#'
#' # However, unless they are named, they won't be useful.
#' df %>%
#'   get_centroids(c(F1, F2), .fns = c(`med` = median, `average` = mean), speaker, vowel)
get_centroids <- function(df, .cols, ..., .fns = median)  {
  df %>%
    group_by(...) %>%
    summarize(across({{.cols}}, eval(substitute(.fns)))) %>%
    ungroup()
}
