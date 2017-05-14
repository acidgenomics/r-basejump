#' basejump
#'
#' Base functions for bioinformatics and R package development.
"_PACKAGE"



#' Prepare the NAMESPACE.
#'
#' @keywords internal
#' @export
#'
#' @seealso
#' - [reexports].
#' - [tidy_verbs()].
namespace <- function() {
    globalVariables(
        c(".",
          "arrange",
          "biocLite",
          "filter",
          "mutate",
          "select"))
}
namespace()
