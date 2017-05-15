#' Import generic [tidyverse](http://tidyverse.org/) verbs to environment.
#'
#' Avoid NAMESPACE collisions with [Bioconductor](https://www.bioconductor.org)
#' and other S4 generics.
#'
#' @keywords internal
#'
#' @param envir Environment to assign the verbs.
#'
#' @export
#'
#' @seealso
#' - [tidyverse style guide](http://style.tidyverse.org/).
#' - [dplyr::arrange()].
#' - [dplyr::filter()].
#' - [dplyr::mutate()].
#' - [dplyr::select()].
import_tidy_verbs <- function(envir = parent.frame()) {
    assign("arrange", dplyr::arrange, envir = envir)
    assign("filter", dplyr::filter, envir = envir)
    assign("mutate", dplyr::mutate, envir = envir)
    assign("select", dplyr::select, envir = envir)
}
