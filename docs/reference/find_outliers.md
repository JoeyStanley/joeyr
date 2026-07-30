# Detect outliers

This is an implementation of the Mahalanobis Distance that is less
sensitive to outliers, first implemented in Stanley (2020). Instead of a
blanket filter applying all at once, it iteratively removes points one
at a time until a predetermined proportion of data has been removed.

## Usage

``` r
find_outliers(..., keep = 0.95, verbose = FALSE)
```

## Arguments

- ...:

  A list of columns in your data that should be included when
  calculating the Mahalanobis distance. The column names should not be
  in quotes. For vowel data, you typically include F1 and F2. You may
  also want to include F3, duration, and any other continuous variable.

- keep:

  A number indicating the proportion of data (per group) to keep. By
  default, it's 0.95 so it keeps 95% of the data and filters out 5%.

- verbose:

  logical, `FALSE` by default. If `TRUE`, you'll get a message for every
  group with less than 20 tokens saying that there weren't enough tokens
  to remove outliers. Can be quite verbose if there are many
  speakers/vowels.

## Value

A vector of TRUE/FALSE values. They are in the same order as the
original dataset. Observations that are considered outliers have the
value TRUE. It is easiest to work with this by appending this vector to
your dataframe.

## Details

The Mahalanobis distance function is somewhat sensitive to outliers, so
if there are extreme values in your data, the mean value will be
off-center from the centroid of your observations. Consequently, the
Mahalanobis Distances will be based on this off-center points, which is
probably not desirable. This function alleviates this sensitivity to
outliers by implementing a one-at-a-time method.

When you run this function, it will first calculate Mahalanobis distance
from the mean of all values. It detects the point furthest from the mean
and removes it. Then, it recalculates the Mahalanobis distance with the
remaining values and again removes the furthest value. It continues this
recalculation-and-removal method until a predetermined proportion of
values has been removed.

## Note

While not required, you should typically "group" your data before
applying this function. For example, you can group your data by speaker
and vowel so that the function applies independently for each vowel for
each speaker. I normally do this with `dplyr::group_by(speaker, word)`

Note also that in American English, allophonic variation of some vowels
is so great that grouping by vowel may not be enough. If you're working
with /u/ for example, it's a good idea to split it into three groups:
post-coronal, pre-lateral, and elsewhere. For /æ/, it's a good idea to
group prenasal tokens separately. If you're using FAVE/DARLA/MFA output,
the NORTH and FORCE classes of words are transcribed with AO, so it's a
good idea to treat those separately. The point is to be mindful of
allophonic variation in your data and that it's a good idea to group the
data by vowel *class* rather than by vowel. You may have to do some
processing before the filter happens to get this to happen. As of
version 0.8 of joeyr, you can now use the `code_allophones` function to
automatically classify your data into allophones.

Finally, be aware that no tokens will be marked as outliers if the are
not a sufficient number of tokens. So if you want to remove 5 you'll
need to have at least 20 tokens in a group for an outlier to be found
within that group. A message will let you know if this happens.
Unfortunately, the function cannot help determine which group(s) the
message came from, but you can find out with
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html).
See the examples.

## References

If you use this function, you can refer to it as something like "the
Modified Mahalanobis Distance method implemented in Stanley (2020)."

Stanley, Joseph A. "The Absence of a Religiolect among Latter-Day Saints
in Southwest Washington." In *Speech in the Western States: Volume 3,
Understudied Varieties*, by Valerie Fridland, Alicia Beckford Wassink,
Lauren Hall-Lew, and Tyler Kendall, 95–122. Publication of the American
Dialect Society 105. Durham, NC: Duke University Press, 2020.
<https://doi.org/10.1215/00031283-8820642>.

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))
df <- joeysvowels::coronals

# You can output the data to a column called something like "is_outlier" and
# then filter out values that are TRUE.
df %>%
   group_by(vowel) %>%
   mutate(is_outlier = find_outliers(F1, F2, keep = 0.95)) %>%
   filter(!is_outlier)
#> # A tibble: 13,731 × 14
#> # Groups:   vowel [13]
#>    vowel_id start   end     t percent    F1    F2    F3    F4 word  pre   vowel
#>       <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <chr> <fct>
#>  1        1  2.06  2.41  2.06       0  387. 1701. 2629. 3164. snoʊz sn    GOAT 
#>  2        1  2.06  2.41  2.07       5  483. 1591. 2454. 3310. snoʊz sn    GOAT 
#>  3        1  2.06  2.41  2.09      10  525. 1466. 2526. 3343. snoʊz sn    GOAT 
#>  4        1  2.06  2.41  2.13      20  530. 1297  2616. 3330  snoʊz sn    GOAT 
#>  5        1  2.06  2.41  2.14      25  497. 1223. 2562. 3280. snoʊz sn    GOAT 
#>  6        1  2.06  2.41  2.16      30  461. 1172. 2559. 3252  snoʊz sn    GOAT 
#>  7        1  2.06  2.41  2.18      35  414. 1120  2625. 3247. snoʊz sn    GOAT 
#>  8        1  2.06  2.41  2.20      40  423  1072. 2655. 3175. snoʊz sn    GOAT 
#>  9        1  2.06  2.41  2.22      45  396. 1074  2623. 3248. snoʊz sn    GOAT 
#> 10        1  2.06  2.41  2.23      50  368. 1018. 2602. 3168. snoʊz sn    GOAT 
#> # ℹ 13,721 more rows
#> # ℹ 2 more variables: fol <chr>, is_outlier <lgl>

# Alternatively, you can skip a step and just keep the data that are not
# outliers.
df %>%
   group_by(vowel) %>%
   filter(!find_outliers(F1, F2))
#> # A tibble: 13,731 × 13
#> # Groups:   vowel [13]
#>    vowel_id start   end     t percent    F1    F2    F3    F4 word  pre   vowel
#>       <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <chr> <fct>
#>  1        1  2.06  2.41  2.06       0  387. 1701. 2629. 3164. snoʊz sn    GOAT 
#>  2        1  2.06  2.41  2.07       5  483. 1591. 2454. 3310. snoʊz sn    GOAT 
#>  3        1  2.06  2.41  2.09      10  525. 1466. 2526. 3343. snoʊz sn    GOAT 
#>  4        1  2.06  2.41  2.13      20  530. 1297  2616. 3330  snoʊz sn    GOAT 
#>  5        1  2.06  2.41  2.14      25  497. 1223. 2562. 3280. snoʊz sn    GOAT 
#>  6        1  2.06  2.41  2.16      30  461. 1172. 2559. 3252  snoʊz sn    GOAT 
#>  7        1  2.06  2.41  2.18      35  414. 1120  2625. 3247. snoʊz sn    GOAT 
#>  8        1  2.06  2.41  2.20      40  423  1072. 2655. 3175. snoʊz sn    GOAT 
#>  9        1  2.06  2.41  2.22      45  396. 1074  2623. 3248. snoʊz sn    GOAT 
#> 10        1  2.06  2.41  2.23      50  368. 1018. 2602. 3168. snoʊz sn    GOAT 
#> # ℹ 13,721 more rows
#> # ℹ 1 more variable: fol <chr>

# In some cases, you might not have enough data. In this case, a warning
# message will appear.
df %>%
    filter(percent == 50) %>%
    group_by(vowel) %>%
    mutate(is_outlier = find_outliers(F1, F2, keep = 0.95))
#> # A tibble: 779 × 14
#> # Groups:   vowel [13]
#>    vowel_id start   end     t percent    F1    F2    F3    F4 word  pre   vowel 
#>       <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <chr> <fct> 
#>  1        1  2.06  2.41  2.23      50  368. 1018. 2602. 3168. snoʊz sn    GOAT  
#>  2        2  3     3.36  3.18      50  377. 2032. 2761. 3408. deɪz  d     FACE  
#>  3        3  3.99  4.35  4.17      50  429. 1004. 2625. 3226. zɔɪd  z     CHOICE
#>  4        4  4.94  5.19  5.07      50  314. 2179. 2915. 3380. teɪd  t     FACE  
#>  5        5  5.91  6.17  6.04      50  274. 1378. 2235. 3270. zudz  z     GOOSE 
#>  6        7  8.13  8.4   8.26      50  350. 1624. 2485. 3384. stʊz  st    FOOT  
#>  7        8  9.16  9.41  9.28      50  313. 2181. 2863. 3205. heɪdz h     FACE  
#>  8        9 10.5  10.8  10.6       50  255. 1333. 2313. 3212. zuz   z     GOOSE 
#>  9       10 11.6  11.9  11.8       50  585   969. 2813. 3405  zɔd   z     THOUG…
#> 10       11 12.6  13.1  12.8       50  527. 1201. 2733. 3434. sɔz   s     THOUG…
#> # ℹ 769 more rows
#> # ℹ 2 more variables: fol <chr>, is_outlier <lgl>
# You can find out which groups have less than 20 tokens with `dplyr::count()`:
df %>%
    filter(percent == 50) %>%
    group_by(vowel) %>%
    count()
#> # A tibble: 13 × 2
#> # Groups:   vowel [13]
#>    vowel       n
#>    <fct>   <int>
#>  1 LOT        55
#>  2 TRAP       59
#>  3 PRICE      61
#>  4 MOUTH      76
#>  5 FACE       44
#>  6 DRESS      37
#>  7 FLEECE     19
#>  8 KIT        57
#>  9 GOAT       76
#> 10 THOUGHT    72
#> 11 CHOICE     76
#> 12 GOOSE      71
#> 13 FOOT       76
```
