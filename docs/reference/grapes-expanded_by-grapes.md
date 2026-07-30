# Expand a range

This function expands ranges by some value. Used in my dissertation code
for axis ranges.

## Usage

``` r
range %expanded_by% expansion
```

## Arguments

- range:

  A numeric vector. The highest and lowest values will be used as the
  range.

- expansion:

  A number. This number will be added to the highest and subtracted from
  the lowest values of the range

## Value

A list of length 2

## Examples

``` r
c(1,5) %expanded_by% 1
#> [1] 0 6
```
