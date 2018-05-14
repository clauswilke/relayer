#' Rethinking layers in ggplot2
#'
#' @name relayer
#' @docType package
#' @import ggplot2
#' @import rlang
NULL


# *************************************************
#                     Setup
# *************************************************

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note: The package \"relayer\" is highly experimental. Use at your own risk.")
}
