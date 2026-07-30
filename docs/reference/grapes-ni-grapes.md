# The "Not In" function

This returns `TRUE` if values are *not* in a list. It's the opposite of
`%in%`. This is particularly good when subsetting and you only want to
remove a few things (like diphthongs). You can remove those rather than
specifying all monophthongs you want to keep.

## Usage

``` r
x %ni% y
```

## Arguments

- x:

  A vector, presumably a subset of "y"

- y:

  A vector

## Value

A list of logicals indicating whether the objects in `x` are not
contained in `y`.

## Details

Note that what this function does can easily be accomplished with
`!x %in% y`, but I like the "not in" aspect. Probably a holdover from my
Perl days...

You can also define this function as `` `%ni%` <- Negate(`%in%`) `` but
when looking through the documentation on %in%, I found that it might be
better to modify the formal definition there instead.

Credit to [this Stack Overflow
question](https://stackoverflow.com/questions/24660864/declaring-special-infix-functions-in-r-packages)
for showing how to get this to work in an R package.

## Examples

``` r
1:10 %in% c(1,3,5,9)
#>  [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
1:10 %ni% c(1,3,5,9)
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
```
