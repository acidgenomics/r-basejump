globalVariables(".")

# Trailing number is to match cellranger output.
barcodePattern <- ")_([ACGT_]{6,})(_[0-9]+)?$"

# Single-cell clustering columns that can mess up `sampleData()` return.
# We're grep matching against these camel case variants here to automatically
# sanitize `colData()` into sample-level `sampleData()`.
clusterCols <- c(
    "^ident$",
    "^origIdent$",
    "^res[.0-9]+$",
    "^sScore$",
    "^g2mScore$",
    "^phase$"
)

# Note optional matching of gzip.
extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"

#' Lane Grep Pattern
#' @keywords internal
#' @export
#' @examples
#' lanePattern
lanePattern <- "_L(\\d{3})"

# List item (following HTML convention).
li <- "  -"

# NA strings.
na <- c("", "NA", "#N/A", "NULL", "null")

# Ignore case.
rdataExtPattern <- "\\.(rd[a|ata|s])$"

rdataError <- "R data files must contain `.rda`, `.rds`, or `.RData` extension."

#' Update Message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run `updateObject()` to update your object."
