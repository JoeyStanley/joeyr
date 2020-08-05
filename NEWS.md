# joeyr 0.4

* Switching to pkgdown, which involved some overdue tidying of the package
  - Removed depreciated functions, including `hz`, `bark`, `spread_n`, `move_x_before_y`, `move_x_after_y`, and all the functions related to the old outlier detection procedure (`joey_filter`, `joey_do_pca`, `joey_adjust_cutoff`, `joey_rm_outliers`). 
  - Removed the datasets. They were just added like a week ago, but I decided to move them to a separate package `{joeysvowels}`. 
  - Renamed the .R filenames
* Added a `NEWS.md` file to track changes to the package. 