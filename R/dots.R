#' Extract Dots from Function
#'
#' @export
#'
#' @param ... Objects as dots.
#' @param character `boolean`. Return dots (`...`) as `character`.
#'
#' @return
#' - "`character = FALSE`": `list`. Objects as `name` class. Can return the
#'   object from the `name` with [eval()].
#' - "`character = TRUE`": `character`. Names of the dots.
#'
#' @seealso
#' - `help("dotsMethods", "methods")`.
#' - [tidyverse](http://tidyverse.org) documentation:
#'   - [rlang](http://rlang.tidyverse.org).
#'   - [dplyr utils](https://goo.gl/fhAuak).
#'   - [devtools infrastructure](https://goo.gl/bM5TrP).
#'
#' @examples
#' dots(a, b, c, character = FALSE)
#' dots(a, b, c, character = TRUE)
dots <- function(..., character = FALSE) {
    dots <- eval_bare(substitute(alist(...)))
    assert_is_list(dots)
    assert_is_non_empty(dots)
    assert_has_no_duplicates(dots)
    invisible(lapply(dots, assert_is_name))

    # Convert names (symbols) to character.
    names <- vapply(dots, as.character, character(1L))
    assert_has_no_duplicates(names)

    if (isTRUE(character)) {
        names
    } else {
        dots
    }
}
