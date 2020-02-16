

# Spread_n ----------------------------------------------------------------

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



# Move columns ----------------------------------------------------------------

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








# Color Gradienter -----------------------------------------------------------

#' Color Gradienter
#'
#' Get some number of equidistant colors between two colors.
#'
#' This function takes two colors and a number and returns an n-length list of colors that are all equidistant from one another. This is handy for plotting categorical, ordinal, or binned data but you want to use continuous colors.
#'
#' @param hi A string, in RGB format ("#123456")
#' @param lo A string, in RBG format ("#ffffff"). The default is white.
#' @param shades An integer
#' @return A list of strings, in RGB format, the length of `shades` and that are equidistant from each other from `hi` to `lo`.
#' @examples
#' # By itself, it returns just a list.
#' color_gradienter("#33458F", "#ffffff", 5)
#'
#' # This can be fed into ggplot::sacle_color_manual()
#' df <- data.frame(x = rnorm(50, 0, 1),
#'                  y = rnorm(50, 0, 1),
#'                  color = sample(letters[1:5], 50, replace = TRUE))
#' ggplot(df, aes(x, y, color = color)) +
#'     geom_point(size = 5) +
#'     scale_color_manual(values = color_gradienter(hi = "#33458F", lo = "#ffffff", 5))

# I wrote this for th Shiny app, but I think it's a useful tool to have.
# Currently uses tidyverse functions but it'd be easy to switch over to base R.

color_gradienter <- function(hi, lo = "#ffffff", shades) {

    if (str_detect(hi, "#[0-9a-fA-F]{6}") == FALSE) {
        warning("`hi` must be in RGB format: #123AbC")
    }
    if (str_detect(lo, "#[0-9a-fA-F]{6}") == FALSE) {
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

    tibble(shade_num = 0:intervals) %>%
        mutate(r = ifelse(lo_r <= hi_r, lo_r, hi_r) + abs(hi_r - lo_r) / intervals * shade_num,
               g = ifelse(lo_g <= hi_g, lo_g, hi_g) + abs(hi_g - lo_g) / intervals * shade_num,
               b = ifelse(lo_b <= hi_b, lo_b, hi_b) + abs(hi_b - lo_b) / intervals * shade_num) %>%
        mutate_at(vars(r, g, b), round) %>%
        mutate_at(vars(r, g, b), as.hexmode) %>%
        mutate(rgb = paste0("#", r, g, b)) %>%
        pull(rgb)
}

