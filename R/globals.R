globalVariables(".")



# Note optional matching of gzip
extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"

# Ignore case
rdataExtPattern <- "\\.(rd[a|ata|s])$"

rdataError <- "R data files must contain `.rda`, `.rds`, or `.RData` extension."



#' Separator Bar
#' @export
#' @examples
#' separatorBar
separatorBar <- paste0(
    rep(x = "=", times = options("width")),
    collapse = ""
)



#' Update Message
#' @export
#' @examples
#' updateMessage
updateMessage <- "Run `updateObject()` to update your object"
