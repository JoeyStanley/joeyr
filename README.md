# joeyr

A hodge-podge of useful functions.

## Introduction

This is essentially my sandbox R Package. It has a hodge-podge of
functions that I've used over the past several years. As I work with 
my sociophonetic data, I find myself running the same sorts of procedures
over and over, so I often decide to write a generalized function to take 
care of that stuff in one line within a tidyverse pipeline. 

The functions in `joeyr` can be grouped into about five different
categories: the outlier detecters, sociophonetics functions, `ggplot2` themes, 
the "grapes", and other helpful functions. This README briefly explains them all.

Some of these functions I've generalized into something that others may
find useful. Others are very specific to my own code and workflow so I’m
not sure if they’ll be useful to you. Some functions are documented well
but lots are not. 

For questions, feel free to contact me at <joey_stanley@byu.edu> or on
Bluesky at [@joeystanley.com](https://bsky.app/profile/joeystanley.com).

## Installation

This package can be downloaded to your computer by downloading it from
Github.

``` r
#install.packages("devtools") # <- if not already installed
devtools::install_github("JoeyStanley/joeyr")
```

You can then load it like any other package.

``` r
library(joeyr)
```

## Group 1: The Filter

The main function, and the reason I created the package in the first
place, is `find_outliers()`. It implements a version of the Mahalanobis
Distance, except it does so iteratively. After finding the distances for
each point, it marks the furthest token as an outlier, and then
recalculates the distance based on the remaining points. It continues
this one-at-a-time procedure until a proportion of your data (that
you’ve specified) has been removed.

For more details, see `?find_outliers`. In the future I’ll write a
vignette, blog post, or (perhaps some day) an article about it. For now,
look at the help page or email me.

## Group 2: Sociophonetics functions

Some functions in `joeyr` are useful for sociophonetic analysis. This first set 
includes what are essentially mathematical functions in that they crunch some numbers.

  - `eucl_dist()` calculates the Euclidean Distance, given a pair of *x*
    and *y* (or, more commonly, F1 and F2) coordinates.
    
  - `get_centroids()` is a little shortcut that summarizes data quickly
    and is helpful when plotting medians within ellipses in ggplot2. 

  - `pillai()` calculates the Pillai score. It’s not a complicated process
    in general, but this function simplifies it down quite a bit so it
    doesn’t interrupt your workflow.

  - `tidy_mahalanobis()` is probably my favorite. It’s an implementation
    of the `mahalanobis()` function, which calculates the Mahalanobis
    distance, only it’s meant to work within a `tidyverse` pipeline of
    commands. Like the `pillai()` function, it’s designed so that you can
    get the values you want without interrupting your flow.
    
  - `lbms_index()` allows you to quickly calculate the Low-Back-Merger Shift 
    Index in your data (see Becker 2019; Boberg 2019).
    
There are two functions are more about changing labels for phonemes and 
allophones.
    
  - `switch_transcriptions()` (and its shortcuts including, `arpa_to_wells()`), quickly converts 
    between ARPABET labels and Wells (or Wells-inspired) keywords. 
    
  - `code_allophones()` is a one-liner that adds contextual allophones ("BAN", 
  "BAG", "SPOOL", "TOOT", etc.) to your vowel data.
  
Finally, there is one purely informational function, `OoO()` that serves as reminder
(mostly for myself) for what my recommended Order of Operations is. 
  
There *were* three normalization procedures in this package, but as of June 18, 
2025 with the release of Fruehwald's [tidynorm](https://jofrhwld.github.io/tidynorm/) 
package, I'm recommending people switch to those instead.

## Group 3: `ggplot2` themes

These are currently not documented. The main one is `theme_joey()` which will
produce plots using my own flavor of `theme_bw()`. There are some variants
as well. The other helpful function is `joey_arrow()` which is just a
shortcut for a type of arrow I like when I draw lines on a plot. I'll admit I
don't really use these anymore other than the arrow.

## Group 4: The grapes

“Grapes” refer to the type of R functions that are flanked on either
side by `%`, like `%in%`. The ones here came about while writing my
dissertation (or rather, the code used to analze the data for my
dissertation) and were super helpful for that project.

  - `%ni%` ("not in"") is super handy and is the opposite if `%in%`. I
    don’t use it so much anymore because I’ve figured out how to use `!`
    properly, but I still love it.

  - `%wi%` ("within") was one I wrote to see whether a number is
    within a range. I think tidyverse has `%within%` somewhere in
    its suite of packages, but I wrote this before I knew about it.
    Plus, it’s a little shorter.

  - `%expanded_by%` takes a number and a range and expands the range by
    that much (applied to both sides). So `c(1, 3) %expanded_by% 3`
    produces `c(-2, 6)`. It was helpul when I was automating the plots
    in my dissertation.

## Group 5: Other helpful functions

While writing my dissertation, I found myself doing the same sort of
pipeline over and over so I just made a couple little shortcut functions
to save myself some typing.

  - `color_gradienter()` was needed in my Shiny app, [*The Gazetteer of
    Southern Vowels*](http://lap3.libs.uga.edu/u/jstanley/vowelcharts/).
    I needed a way to take two arbitrary colors and produce an arbitrary
    number of colors at fixed intervals between them. This is helpful
    for plots: do you want to use your favorite color of green and your
    favorite color of blue, but also get three more colors in between
    them for a continuous scale? `color_gradienter()` can do that. I don’t
    know everything about how color is encoded, but it seems to work
    well enough for me, though there may be some weird bugs.

  - `ucfirst()` was a function I copied over from Perl code. It just
    capitalizes the first character of a string. Unlike some other
    functions that are more sophisticaed and transform the text into
    title case, sometimes I just need the very first character only,
    rather than the first character of each word.

    
## Depreciated functions
Previous versions of `{joeyr}` had a couple other tools, but recent versions
of `{tidyr}` and `{dplyr}` have rendered them unnecessary. The following are
no longer part of `{joeyr}`, though you can find the code on GitHub in the 
`R_depreciated` folder.

  - `spread_n()` was perhaps my favorite function. I had a need for using
    the (old) `tidyr::spread()` with multiple `value` columns while
    reshaping formant data. I went to the RStudio forums and someone
    wrote a slick little function for me that I dubbed `spread_n()`.
    (Kieran Healey saw that post and wrote a [blog
    post](https://kieranhealy.org/blog/archives/2018/11/06/spreading-multiple-values/)
    on it\!). The function has since been rendered obsolete with the new
    `tidyr::pivot_wider()` (Kieran Healey
    [wrote](https://kieranhealy.org/blog/archives/2019/10/21/widening-multiple-columns-redux/)
    about that too), so I don’t use `spread_n()` anymore. Perhaps it’ll
    eventually be removed from the package. But I can’t help but think
    that my asking that question had some role in the `pivot_wider()`
    function.

  - `move_x_after_y()` and `move_x_before_y()` are helpful for when you want
    to take a column that you’ve just created and move it before or
    after some existing column. It’s a little buggy when you work with
    columns on the edges of your dataframe, but it worked well for me
    and my dissertation code. Note that once `dplyr` 1.0.0 is released,
    these functions will be phased out since the new
    [`dplyr::relocate`](https://dplyr.tidyverse.org/dev/reference/relocate.html)
    function does what I intended these do to.
    
## Citation

If you use this package, I would appreciate a citation. You can cite it as:

> Stanley, Joseph A. joeyr: Functions for Vowel Data (R package version ###), 2021. https://joeystanley.github.io/joeyr/ 

Specifically, if you use the `find_outliers()` function, you can refer to it as 
something like "the Modified Mahalanobis Distance method implemented in Stanley 
(2020)."

> Stanley, Joseph A. “The Absence of a Religiolect among Latter-Day Saints in 
Southwest Washington.” In Speech in the Western States: Volume 3, Understudied 
Varieties, by Valerie Fridland, Alicia Beckford Wassink, Lauren Hall-Lew, and 
Tyler Kendall, 95–122. Publication of the American Dialect Society 105. Durham, 
NC: Duke University Press, 2020. [https://doi.org/10.1215/00031283-8820642](https://doi.org/10.1215/00031283-8820642).


## Conclusion

That’s `joeyr`. I hope you find it useful!
