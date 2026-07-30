# Within

Here's a very handy "within" range. Checks whether a number is within a
range. Capable of handling lists so that it can apply to a whole column.
Basically I got tired of running a two-part conditional when I wanted
something that was within a range: `if (x > 2 & x < 5)`.

## Usage

``` r
x %wi% range
```

## Arguments

- x:

  A numeric vector.

- range:

  A numeric vector. The highest and lowest values will be used as the
  range.

## Value

A logical vector.

## Examples

``` r
4 %wi% c(1,5)
#> [1] TRUE
c(2, 3) %wi% c(1,5)
#> [1] TRUE TRUE
c(1:5) %wi% c(4,6)
#> [1] FALSE FALSE FALSE  TRUE  TRUE
```
