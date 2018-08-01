globalVariables(".")



# Note optional matching of gzip
extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"

# Ignore case
rdataExtPattern <- "\\.(rd[a|ata|s])$"

rdataError <- "R data files must contain `.rda`, `.rds`, or `.RData` extension."



#' Separator Bar
#' @keywords internal
#' @export
#' @examples
#' cat(separatorBar)
separatorBar <- paste0(
    rep(x = "=", times = getOption("width", 72L)),
    collapse = ""
)



#' Update Message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run `updateObject()` to update your object"
