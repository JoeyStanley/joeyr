# Measure euclidean distance

This function takes the x and y coordinates of two points and finds the
euclidean distance between them. Note that this only works in
two-dimensional data: euclidean distances between points in a
three-dimensional space is not currently supported.

## Usage

``` r
eucl_dist(x1, x2, y1, y2)
```

## Arguments

- x1, x2, y1, y2:

  A number

## Value

The distance between the two points (a number).

## Examples

``` r
eucl_dist(1,2,1,2)
#> [1] 1.414214
```
