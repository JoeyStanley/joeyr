# The negate function -----------------------------------------------------
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
#' through the documentatin on \%in\%, I found that it might be better to
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





# The within function ------------------------------------------------------------------------------------

#' Within
#'
#' Here's a very handy "within" range. Checks whether a number is within a range. Capable of handling lists so that it can apply to a whole column. Requires no additional packages.
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

# The expand_by function ------------------------------------------------------------------------------------

#' Expand a range
#'
#' This function expands ranges by some value. Used in dissertation code for axis ranges. Requires no additional packages.
#'
#' @param range A numeric vector. The highest and lowest values will be used as the range.
#' @param range A number. This number will be added to the highest and subtracted from the lowest values of the range
#' @return A list of length 2
#' @examples
#' c(1,5) %expanded_by% 1
`%expanded_by%` <- function(range, expansion) {
    c(min(range) - expansion,
      max(range) + expansion)

}


# Spread_n ----------------------------------------------------------------

#' spread() on multiple values.
#'
#' Use tidyr::spread but on multiple columns. I haven't been able to get this function to work well when there are two numeric columns I need to work with. All credit goes to "danr" on the RStudio Forums who answered my quesiton [here](https://community.rstudio.com/t/spread-with-multiple-value-columns/5378/2).
#'
#' @examples
#' # Create a data frame with some sample formants. Crucially there are two numeric values for each row.
#' df1 <- data.frame(word = rep(c("dog", "cat", "fish"), 2),
#' formant = rep(c("F1", "F2"), each = 3),
#' hz = c(490, 500, 510, 1590, 1600, 1610),
#' hz_bark = bark(c(490, 500, 510, 1590, 1600, 1610)))
#' # Now spread them with each formant and type of measurement in its own column.
#' df1 %>%
#'    spread_n(formant, c(hz, hz_bark))
#'
#' df2 <- data.frame(speaker = rep(c("Amy", "Bob"), each = 3),
#'                   word = rep(c("dog", "cat", "fish"), 2),
#'                   F1 = c(490, 500, 510, 390, 400, 410),
#'                   F2 = bark(c(1590, 1600, 1610, 1390, 1400, 1410)))
#' df2 %>%
#'     spread_n(word, c(F1, F2))
#' df %>%
#'    spread_n(speaker, c(F1, F2))

# I don't fully understand this, and I will freely admit this is blatantly taken from a question
spread_n <- function(df, key, value) {
    # quote key
    keyq <- rlang::enquo(key)
    # break value vector into quotes
    valueq <- rlang::enquo(value)
    s <- rlang::quos(!!valueq)
    df %>% gather(variable, value, !!!s) %>%
        unite(temp, !!keyq, variable) %>%
        spread(temp, value)
}

# Move columns ----------------------------------------------------------------

# This is from my dissertation code.
# It might be better to change it to move(x, after = y), which would allow also for move(x, before = y).
# Since "move" is a pretty generic name (though unused as far as I can tell), maybe do move_col().
# Also, add functionality to do move_cols(x, y, after = z) for a mass move.

#' Move one column after another
#'
#' This quite simply moves one column after another in a data frame. Just a simple way to rearrange a couple columns without having to use select. It saves a bit of typing and makes it more clear what it is that's happening.
#'
#' @param df A data frame.
#' @param colx A column in that dataframe.
#' @param coly Another column in that dataframe.
#' @return A dataframe, with colx immediately after coly.
#' @examples
#' sample_df <- data.frame(first = 1:3, middle = 4:6, last = 7:9)
#' move_x_after_y(sample_df, last, first)
move_x_after_y <- function(df, colx, coly) {
    # Tidyeval, so that the arguments don't have to be quoted.
    x_expr <- enquo(colx)
    y_expr <- enquo(coly)

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


#' Move one column after another
#'
#' This quite simply moves one column after another in a data frame. Just a simple way to rearrange a couple columns without having to use select. It saves a bit of typing and makes it more clear what it is that's happening.
#'
#' @param df A data frame.
#' @param colx A column in that dataframe.
#' @param coly Another column in that dataframe.
#' @return A dataframe, with colx immediately after coly.
#' @examples
#' sample_df <- data.frame(first = 1:3, middle = 4:6, last = 7:9)
#' move_x_after_y(sample_df, last, first)
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

