

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






