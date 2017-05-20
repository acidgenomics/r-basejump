#' Import [Bioconductor](https://www.bioconductor.org/)-masked generic
#' [tidyverse](http://tidyverse.org/) verbs to environment.
#'
#' Avoid NAMESPACE collisions with [Bioconductor](https://www.bioconductor.org/)
#' BiocGenerics and S4Vectors.
#'
#' @keywords internal
#'
#' @param envir Environment where to assign the verbs.
#'
#' @export
#'
#' @seealso
#' - [BiocGenerics](http://bioconductor.org/packages/release/bioc/html/BiocGenerics.html).
#' - [S4Vectors](http://bioconductor.org/packages/release/bioc/html/S4Vectors.html).
#' - [tidyverse style guide](http://style.tidyverse.org/).
#' - [dplyr](http://dplyr.tidyverse.org/).
#' - [tidyr](http://tidyr.tidyverse.org/).
importTidyVerbs <- function(envir = parent.frame()) {
    assign("arrange",
           dplyr::arrange,
           envir = envir)
    # BiocGenerics
    assign("combine",
           dplyr::combine,
           envir = envir)
    # S4Vectors
    assign("expand",
           tidyr::expand,
           envir = envir)
    assign("filter",
           dplyr::filter,
           envir = envir)
    # S4Vectors
    assign("first",
           dplyr::first,
           envir = envir)
    # S4Vectors
    assign("intersect",
           dplyr::intersect,
           envir = envir)
    assign("mutate",
           dplyr::mutate,
           envir = envir)
    # S4Vectors
    assign("rename",
           dplyr::rename,
           envir = envir)
    assign("select",
           dplyr::select,
           envir = envir)
    # S4Vectors
    assign("setdiff",
           dplyr::setdiff,
           envir = envir)
    # S4Vectors
    assign("setequal",
           dplyr::setequal,
           envir = envir)
    # S4Vectors
    assign("union",
           dplyr::union,
           envir = envir)
}

#' @rdname aliases
#' @usage NULL
#' @export
import_tidy_verbs <- importTidyVerbs
