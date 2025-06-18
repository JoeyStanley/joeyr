# joeyr 0.10 (June 18, 2025)

* The normalization functions (`norm_logmeans`, `norm_anae`, and `norm_deltaF`) have been depreciated for several reasons. The [tidynorm](https://jofrhwld.github.io/tidynorm/) package handles these much more effectively
and efficiently than what I was able to do (see introduction [here](https://jofrhwld.github.io/blog/posts/2025/06/2025-06-16_introducing-tidynorm/)). The three functions now do nothing except give a message to the user recommending that they switch to tidynorm instead. For those who don't want to make the switch, the three 
functions will still exist in this package but have been renamed `joeyr_norm_logmeans`, `joeyr_norm_anae` and `joeyr_norm_deltaF`. This prevents conflicts as well.

* I've added a very simple `OoO` function that reminds me what my recommended order of operations is. 

* I've also removed the `This is the "joeyr" package` message when loading.

# joeyr 0.9.2 (May 1, 2025)

* Updated `code_allophones`
  - MOUTH gets a prelateral allophone now: PROWL. This means the elsewhere allophone is now called BOUT.
  - PRICE gets a prelateral and a prerhotic allophone: CHILD and PRIOR. 
  - Prevoiceless PRICE has been changed from PRICE to BITE. Prevoiced PRICE has been changed from PRIZE to BIDE.

# joeyr 0.9.1 (February 18, 2025)
 
* Fixed two issues with GOOSE allophones in `code_allophones`. 
  - Words like *tool* which are both post-coronal and prelateral, were previously coded as post-coronal. That has been fixed so that they're not prelateral.
  - There wasn't a prerhotic allophone of GOOSE, which caused words like *demure* to be erroneously classified as TOOT. Now, prerhotic GOOSE is CURE. Note that prerhotic FOOT is also CURE.

# joeyr 0.9 (March 27, 2023)

* `get_centroids` added.

# joeyr 0.8.1 (June 8, 2022)

* `norm_logmeans()` has a raised cap before triggering a warning message. 
* `find_outliers()` now as the `verbose` argument which, by default, suppresses the warning message triggered when there is not enough data.

# joeyr 0.8 (May 15, 2022)

* `code_allophones()` now added.
* `norm_logmeans()` now added, which does a log-means normalization based on Barreda & Nearey (2018).
* `norm_anae()` updated. The `use_telser_g` argument has been changed to simply `g`. Previously, if you wnated to calculate g internally, you'd do `use_telsur_g = FALSE` and that code didn't work. Now, you'd say `g = "calculate"`, and that does work this time. Additionally, some new functionality is that `g` can be set to any arbitrary value, such as zero: `g = 0`. 

# joeyr 0.7 (July 26, 2021)

* `arpa_to_keywords()` has been replaced with the more robust and flexible `switch_transcriptions`. 

# joeyr 0.6.3 (June 3, 2021)

* `manova_p()` now added.

# joeyr 0.6.2 (February 6, 2021)

* `norm_anae()` now uses the Telsur G by default. This can be suppressed and a new G value can be calculated using the data (the old default behavior), by specifying `use_telsur_g = FALSE`.
* `norm_anae()` now requires the data to be grouped by speaker prior to running. This makes it more consistent with the other normalization procedures in this package.
* `wells_to_arpa()` was created, as an inverse function to `arpa_to_wells()` because I needed it one time.
* `norm_deltaF()` has been fixed so that including F4 now works. Additional arguments have been added as well.  

# joeyr 0.6.1 (September 30, 2020)

* `pillai()` and `tidy_mahalanobis()` get much clearer examples and much more documentation.
* `arpa_to_keywords()` no longer crashes if a dataset didn't have all vowels present.

# joeyr 0.6 (September 27, 2020)

* `arpa_to_keywords()` and `arpa_to_wells()` added.

# joeyr 0.5 (September 24, 2020)

* Example code now uses the the `joeysvowels` package, a remote dependency.
* `norm_anae()` is now more robust (thanks, Thomas!)

# joeyr 0.4 (August 5, 2020)

* Switching to pkgdown, which involved some overdue tidying of the package
  - Removed depreciated functions, including `hz`, `bark`, `spread_n`, `move_x_before_y`, `move_x_after_y`, and all the functions related to the old outlier detection procedure (`joey_filter`, `joey_do_pca`, `joey_adjust_cutoff`, `joey_rm_outliers`). 
  - Removed the datasets. They were just added like a week ago, but I decided to move them to a separate package `{joeysvowels}`. 
  - Renamed the .R filenames
* Added a `NEWS.md` file to track changes to the package. 

# joeyr 0.5--0.8

* Apparently I didn't take careful enough notes. My bad.
