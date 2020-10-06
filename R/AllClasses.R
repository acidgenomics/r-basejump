#' All classes
#' @include AllGlobals.R
#' @noRd
NULL



## > showClass("missingOrNULL")
setClassUnion(name = "missingOrNULL", members = c("missing", "NULL"))
