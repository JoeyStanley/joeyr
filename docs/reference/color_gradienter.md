# Color Gradienter

Get some number of equidistant colors between two colors.

## Usage

``` r
color_gradienter(hi, lo = "#ffffff", shades)
```

## Arguments

- hi:

  A string, in RGB format ("#123456")

- lo:

  A string, in RBG format ("#ffffff"). The default is white.

- shades:

  An integer

## Value

A list of strings, in RGB format, the length of \`shades\` and that are
equidistant from each other from \`hi\` to \`lo\`.

## Details

This function takes two colors and a number and returns an n-length list
of colors that are all equidistant from one another. This is handy for
plotting categorical, ordinal, or binned data but you want to use
continuous colors.

Note, this is a little buggy and may not work as expected. I don't know
enough about how colors are handled to fix bugs. Please ese with
caution.

## Examples

``` r
library(ggplot2)
# By itself, it returns just a list.
color_gradienter("#33458F", "#ffffff", 5)
#> [1] "#33458f" "#6674ab" "#99a2c7" "#ccd0e3" "#ffffff"

# This can be fed into ggplot::scale_color_manual()
df <- data.frame(x = rnorm(50, 0, 1),
                 y = rnorm(50, 0, 1),
                 color = sample(letters[1:5], 50, replace = TRUE))
ggplot(df, aes(x, y, color = color)) +
    geom_point(size = 5) +
    scale_color_manual(values = color_gradienter(hi = "#33458F", lo = "#ffffff", 5))
```
