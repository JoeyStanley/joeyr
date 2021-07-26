# joeyr 0.7

* `arpa_to_keywords()` has been replaced with the more robust and flexible `switch_transcriptions`. 

# joeyr 0.6.3

* `manova_p()` now added.

# joeyr 0.6.2

* `norm_anae()` now uses the Telsur G by default. This can be suppressed and a new G value can be calculated using the data (the old default behavior), by specifying `use_telsur_g = FALSE`.
* `norm_anae()` now requires the data to be grouped by speaker prior to running. This makes it more consistent with the other normalization procedures in this package.
* `wells_to_arpa()` was created, as an inverse function to `arpa_to_wells()` because I needed it one time.
* `norm_deltaF()` has been fixed so that including F4 now works. Additional arguments have been added as well.  

# joeyr 0.6.1

* `pillai()` and `tidy_mahalanobis()` get much clearer examples and much more documentation.
* `arpa_to_keywords()` no longer crashes if a dataset didn't have all vowels present.

# joeyr 0.6

* `arpa_to_keywords()` and `arpa_to_wells()` added.

# joeyr 0.5

* Example code now uses the the `joeysvowels` package, a remote dependency.
* `norm_anae()` is now more robust (thanks, Thomas!)

# joeyr 0.4

* Switching to pkgdown, which involved some overdue tidying of the package
  - Removed depreciated functions, including `hz`, `bark`, `spread_n`, `move_x_before_y`, `move_x_after_y`, and all the functions related to the old outlier detection procedure (`joey_filter`, `joey_do_pca`, `joey_adjust_cutoff`, `joey_rm_outliers`). 
  - Removed the datasets. They were just added like a week ago, but I decided to move them to a separate package `{joeysvowels}`. 
  - Renamed the .R filenames
* Added a `NEWS.md` file to track changes to the package. 