# Refer to S4Vectors code for example of how to reexport classes and methods.
# https://github.com/Bioconductor/S4Vectors/blob/master/NAMESPACE

library(tidyverse)

# Export capitalized, upper camel functions first.
Sys.setlocale("LC_COLLATE", "C")

# Get a vector of the functions reexported in the packages.
generics <- ls(getNamespaceInfo(ns = "basejump.generics", which = "exports"))
reexports <- c(
    "%>%",
    "setAs",
    "setClass",
    generics
)

ns <- getNamespaceInfo(ns = "basejump", which = "imports")
ns$base <- NULL
# Get only the named entries in list.
ns <- ns[which(names(ns) != "")]
# Remove `.__` entries.
ns <- lapply(
    X = ns,
    FUN = function(x) {
        sort(as.character(x[!grepl("^.__", x)]))
    }
)
# Filter the re-exported functions.
ns <- mapply(
    funs = ns,
    pkg = names(ns),
    FUN = function(funs, pkg) {
        if (pkg %in% c("basejump.generics", "magrittr")) {
            funs
        } else {
            funs[!funs %in% reexports]
        }
    }
)



# roxygen2 =====================================================================
# This method is preferred because it automatically creates reexport
# documentation in `man/reexports.Rd`.
# #' @importFrom package function
# #' @export
# package::function
roxygen <- mapply(
    fun = ns,
    package = names(ns),
    FUN = function(fun, package) {
        vapply(
            X = fun,
            FUN = function(fun) {
                # TODO Improve backtick escape method here.
                paste0(
                    "#' @importFrom ", package, " ", fun, "\n",
                    "#' @export\n",
                    package, "::", "`", fun, "`"
                )
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        )
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
)

roxygen %>%
    unlist() %>%
    paste(collapse = "\n\n") %>%
    write_lines("R/reexports.R")

devtools::document()
