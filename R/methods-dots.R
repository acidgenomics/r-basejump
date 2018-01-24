#' Extract Dots from Function
#'
#' @rdname dots
#' @name dots
#' @family Function Utilities
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
NULL



# Methods ======================================================================
#' @rdname dots
#' @importFrom rlang eval_bare
#' @export
setMethod(
    "dots",
    signature("..." = "ANY"),
    function(..., character) {
        dots <- eval_bare(substitute(alist(...)))
        if (length(dots) == 0L) {
            abort("No dots to return")
        }
        isName <- vapply(dots, is.symbol, logical(1L))
        if (any(!isName)) {
            abort("Dots cannot contain arguments")
        }
        names <- vapply(dots, as.character, character(1L))
        dupes <- which(setNames(duplicated(names), names))
        if (length(dupes) > 0L) {
            names <- unique(names)
            abort("Duplicate dots: ", toString(names(dupes)))
        }
        if (isTRUE(character)) {
            names
        } else {
            dots
        }
    })
