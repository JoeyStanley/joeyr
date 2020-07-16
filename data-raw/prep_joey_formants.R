## code to prepare `joey_formants` dataset

joey_formants <- read.csv("http://joeystanley.com/data/joey.csv")
usethis::use_data(joey_formants, overwrite = TRUE)
