#' Extract Dots from Function
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @param ... Objects as dots.
#' @param character Return dots (`...`) as `character` vector.
#'
#' @return
#' - For "`character = FALSE`": `list` containing objects as names. Can return
#'   object from the stored `name` class with [eval()].
#' - For "`character = TRUE`": `character` containing the dot names.
#' @export
#'
#' @seealso
#' - `help("dotsMethods", "methods")`.
#' - [tidyverse](http://tidyverse.org) code:
#'   [rlang](http://rlang.tidyverse.org)
#'   [dplyr utils](https://goo.gl/fhAuak),
#'   [devtools infrastructure](https://goo.gl/bM5TrP).
#'
#' @examples
#' # names
#' dots(mtcars, starwars)
#'
#' # character
#' dots(mtcars, starwars, character = TRUE)
dots <- function(..., character = FALSE) {
    dots <- eval_bare(substitute(alist(...)))
    assert_is_list(dots)
    assert_is_non_empty(dots)
    assert_has_no_duplicates(dots)
    invisible(lapply(dots, assert_is_name))

    # Convert names (symbols) to character vector
    names <- vapply(dots, as.character, character(1L))
    assert_has_no_duplicates(names)

    if (isTRUE(character)) {
        names
    } else {
        dots
    }
}
