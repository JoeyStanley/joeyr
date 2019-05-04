# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("This is the \"joeyr\" package.")
}


# TODO: If a custom column name is supplied to F1 or F2, remove the temp F1 and F2 columns.


