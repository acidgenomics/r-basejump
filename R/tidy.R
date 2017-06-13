#' S4 generic variants of [tidyverse](http://tidyverse.org/) verbs
#'
#' Avoid NAMESPACE collisions with [Bioconductor](https://www.bioconductor.org/)
#' generics by using [setMethod()] to relevant `signature`.
#'
#' @rdname tidy
#' @name tidy
#' @aliases tidyverse
#' @docType methods
#'
#' @param object Primary object.
#' @param x Primary object.
#' @param y Secondary object.
#' @param ... Passthrough parameters.
#'
#' @seealso
#' - [Bioconductor](https://www.bioconductor.org/):
#'     [Biobase], [BiocGenerics], S4Vectors.
#' - [tidyverse](http://tidyverse.org/):
#'     [dplyr], [httr], [tidyr].
NULL
