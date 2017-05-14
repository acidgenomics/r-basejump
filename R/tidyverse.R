#' Prefixed [tidyverse](http://tidyverse.org/) grammar alternates.
#'
#' Assign generic tidyverse verbs to the working environment.
#'
#' Avoid NAMESPACE collisions with [Bioconductor](https://www.bioconductor.org)
#' and other S4 generics.
#'
#' @rdname tidyverse
#'
#' @param envir Environment to assign the tidy verbs.
#' @param .data Tibble. All main verbs are S3 generics.
#' @param ... Comma separated list of unquoted variable names.
#'
#' @export
#'
#' @seealso
#' - [tidyverse style guide](http://style.tidyverse.org/)
#' - [dplyr::arrange()]
#' - [dplyr::filter()]
#' - [dplyr::mutate()]
#' - [dplyr::select()]
tidy_verbs <- function(envir = parent.frame()) {
    assign("arrange", tbl_arrange, envir = envir)
    assign("filter", tbl_filter, envir = envir)
    assign("mutate", tbl_mutate, envir = envir)
    assign("select", tbl_select, envir = envir)
}




#' @rdname tidyverse
#' @description Arrange rows by variables.
#' @export
dplyr::arrange -> tbl_arrange



#' @rdname tidyverse
#' @description Return rows with matching conditions.
#' @export
dplyr::filter -> tbl_filter



#' @rdname tidyverse
#' @description Add new variables.
#' @export
dplyr::mutate -> tbl_mutate



#' @rdname tidyverse
#' @description Select/rename variables by name.
#' @export
dplyr::select -> tbl_select
