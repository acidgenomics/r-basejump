#' Extract Dots from Function
#'
#' @export
#'
#' @param ... Objects as dots.
#' @param character `logical(1)`. Return dots (`...`) as `character`.
#'
#' @return
#' - "`character = FALSE`": `list`. Objects as `name` class. Can return the
#'   object from the `name` with `eval`.
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
    assert(
        is.list(dots),
        hasLength(dots),
        hasNoDuplicates(dots)
    )

    # Provide an informative error message when a user attempts to accidentally
    # use standard evaluation with quotation.
    if (!all(vapply(
        X = dots,
        FUN = is.symbol,
        FUN.VALUE = logical(1L)
    ))) {
        stop(paste(
            "This function uses non-standard evaluation (NSE).",
            "Dot objects must be unquoted.",
            "",
            "More details on NSE:",
            "- https://cran.r-project.org/package=lazyeval",
            "- https://dplyr.tidyverse.org/articles/programming.html",
            "- http://adv-r.had.co.nz/Computing-on-the-language.html",
            sep = "\n"
        ), call. = FALSE)
    }

    # Convert names (symbols) to character.
    names <- vapply(
        X = dots,
        FUN = as.character,
        FUN.VALUE = character(1L)
    )
    assert(hasNoDuplicates(names))
    if (isTRUE(character)) {
        names
    } else {
        dots
    }
}
