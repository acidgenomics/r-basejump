#' British spelling variants
#'
#' British English function name variants (e.g. [summarise()] instead of
#' [summarize()]).
#'
#' @rdname british_aliases
#' @name british_aliases
#' @keywords internal
NULL



#' camelCase aliases
#'
#' Function name variants formatted in `camelCase`.
#'
#' @rdname camel_aliases
#' @name camel_aliases
#' @keywords internal
NULL



#' snake_case aliases
#'
#' Function name variants formatted in `snake_case`.
#'
#' @rdname snake_aliases
#' @name snake_aliases
#' @keywords internal
NULL



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
