#' Extract Dots from Function
#'
#' @family Function Utilities
#'
#' @importFrom rlang eval_bare
#'
#' @param ... Objects as dots.
#' @param character Return dots (`...`) as character vector.
#'
#' @return
#' - For `character = FALSE`: [list] containing objects as names. Can return
#'   object from the stored [name] class with [eval()].
#' - For `character = TRUE`: Character vector.
#' @export
#'
#' @seealso
#' - `help("dotsMethods", "methods")`
#' - `browseVignettes("rlang")`.
#' - [rlang](http://rlang.tidyverse.org) functions:
#'       [rlang::quos()],
#'       [rlang::dots_definitions()],
#'       [rlang::dots_values()],
#'       [rlang::eval_tidy()],
#'       [rlang::eval_bare()].
#' - [tidyverse](http://tidyverse.org) code:
#'       [dplyr utils](https://goo.gl/fhAuak),
#'       [devtools infrastructure](https://goo.gl/bM5TrP)
#'
#' @examples
#' dots(mtcars, starwars)
#' dots(mtcars, starwars, character = TRUE)
dots <- function(..., character = FALSE) {
    dots <- eval_bare(substitute(alist(...)))
    assert_is_list(dots)
    assert_is_non_empty(dots)
    assert_has_no_duplicates(dots)
    invisible(lapply(dots, assert_is_name))

    # Convert names (symbols) to character vector
    names <- vapply(dots, as.character, character(1L))

    # Abort on duplicate detection
    dupes <- names[which(setNames(duplicated(names), names))]
    if (length(dupes) > 0L) {
        abort(paste("Duplicate dots:", toString(dupes)))
    }

    if (isTRUE(character)) {
        names
    } else {
        dots
    }
}
