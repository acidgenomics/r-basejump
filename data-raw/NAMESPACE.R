# Need to quote functions containing `%>%`, `<-`.

# Export capitalized, upper camel functions first.
Sys.setlocale("LC_COLLATE", "C")

ns <- getNamespaceInfo(ns = "basejump", which = "imports")
# Get only the named entries in list.
ns <- ns[which(names(ns) != "")]
# Remove `.__` entries.
ns <- lapply(
    X = ns,
    FUN = function(x) {
        x[!grepl("^.__", x)]
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
                # FIXME Improve backtick escape method here.
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
cat(unlist(roxygen), sep = "\n\n")



# Manual addition to NAMESPACE =================================================
# This method is simple but won't add documentation.
x <- unlist(ns)
x <- sort(unique(as.character(x)))
text <- paste0("export(\"", x, "\")")
cat(text, sep = "\n")
