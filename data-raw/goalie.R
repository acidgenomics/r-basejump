# Re-export functions from goalie package.

# Export capitalized, upper camel functions first.
Sys.setlocale("LC_COLLATE", "C")

pkg <- "goalie"
funs <- getNamespaceInfo(ns = "goalie", which = "exports") %>%
    ls() %>%
    setdiff(c("%>%"))
roxygen <- lapply(
    X = funs,
    pkg = pkg,
    FUN = function(fun, pkg) {
        vapply(
            X = fun,
            FUN = function(fun) {
                # TODO Improve backtick escape method here.
                paste0(
                    "#' @importFrom ", pkg, " ", fun, "\n",
                    "#' @export\n",
                    pkg, "::", "`", fun, "`"
                )
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        )
    }
)

roxygen %>%
    unlist() %>%
    paste(collapse = "\n\n") %>%
    readr::write_lines(
        x = .,
        path = file.path("R", paste0(pkg, ".R"))
    )

devtools::document()
