# Barks and Hz ------------------------------------------------------------

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


# Euclidean Distance ------------------------------------------------------

#' Measure euclidean distance
#'
#' This function takes the x and y coordinates of two points and finds
#' the euclidean distance between the two.
#'
#' @param x1,x2,y1,y2 A number
#' @return The distance between the two points (a number).
#' @examples
#' eucl_dist(1,2,1,2)

eucl_dist <- function (x1, x2, y1, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
}




# A custom Pillai function that is tidyverse-compatible (I've used this before). This one takes the same arguments as `manova` but returns just the pillai score rather than the entire model. Requires no additional packages.


#' Calculate Pillai scores
#'
#' This is just a shortcut to run an MANOVA and return just the pillai score. 
#'
#' @examples
#' pillai(cbind(F1, F2) ~ vowel + fol_seg, data = df)
#' 
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



# TO ADD ----------------------


# library(lme4)
# fit <- glmer(realization ~ age + sex + corpus + (1|speaker) + (1|word), data = thr, family = "binomial")
# summary(fit)
# coef_to_df(fit) %>%
#     filter(variable == "speaker") %>%
#     arrange(-intercept) %>%
#     print()
# coef_to_df(fit) %>%
#     ggplot(aes(intercept)) + 
#     geom_histogram(binwidth = 1)

coef_to_df <- function(fit) {
    coef_list <- coef(fit)
    
    i <- 1
    output <- list(seq_along(names(coef_list)))
    for(rand_variable in names(coef_list)) {
        output[[i]] <- coef_list[rand_variable] %>%
            as.data.frame() %>%
            mutate(level = rownames(.)) %>%
            rename_all(function(x) {str_replace(x, paste0(rand_variable, "."), "")}) %>%
            rename(intercept = .Intercept.) %>%
            mutate(variable = rand_variable) %>%
            select(variable, level, everything())
        i <- i + 1
    }
    output %>%
        bind_rows() %>%
        as_tibble()
}





# This function is a tidyverse-compatible version of the `mahalanobis` function. It just makes it easier to include it as part of a `dplyr::mutate`. One small quirk, if there are fewer than 5 measurements, it returns them all as having a distance of zero. Prevents some errors that way. Requires the MASS package to be install, but does not load it.

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







# I’ll also define some new functions that simply take a fitted model and extract either the coefficients or the t-values.
get_coefs <- function(fit) {
    summary(fit)$coefficients %>%
        as.data.frame() %>%
        rownames_to_column("variable") %>%
        mutate(variable = fct_recode(variable, intercept = "(Intercept)")) %>%
        select(variable, `Estimate`) %>%
        spread(variable, `Estimate`)
}

#In the case of the t-values, if they’re less than 2, I turn them into NaN so it’s easier to spot the significant effects.
get_t_values <- function(fit) {
    summary(fit)$coefficients %>%
        as.data.frame() %>%
        rownames_to_column("variable") %>%
        mutate(variable = fct_recode(variable, intercept = "(Intercept)")) %>%
        select(variable, `t value`) %>%
        spread(variable, `t value`) %>%
        mutate_at(vars(-intercept), funs(if_else(abs(.) < 2, NaN, .)))
}

# Also get p-values.
get_p_values <- function(fit) {
    summary(fit)$coefficients %>%
        as.data.frame() %>%
        rownames_to_column("variable") %>%
        mutate(variable = fct_recode(variable, intercept = "(Intercept)")) %>%
        rename("p_value" = `Pr(>|t|)`) %>%
        select(variable, p_value) %>%
        spread(variable, p_value)
}

# And significance stars.
get_significance <- function(fit) {
    summary(fit)$coefficients %>%
        as.data.frame() %>%
        rownames_to_column("variable") %>%
        mutate(variable = fct_recode(variable, intercept = "(Intercept)")) %>%
        rename("p_value" = `Pr(>|t|)`) %>%
        select(variable, p_value) %>%
        spread(variable, p_value) %>%
        mutate_at(vars(-intercept), funs(case_when(. < 0.001 ~ "***",
                                                   . < 0.01  ~ "**",
                                                   . < 0.05  ~ "*",
                                                   . < 0.1   ~ ".", 
                                                   TRUE ~ "")))
}