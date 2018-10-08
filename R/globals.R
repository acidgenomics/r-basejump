globalVariables(".")

packageVersion <- packageVersion("basejump")

#' Cache URL
#' @keywords internal
#' @export
#' @examples
#' basejumpCacheURL
basejumpCacheURL <- paste0(
    "http://basejump.seq.cloud/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
)

# Trailing number is to match cellranger output.
barcodePattern <- ")_([ACGT_]{6,})(_[0-9]+)?$"

# Note optional matching of gzip.
extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"

#' Lane Grep Pattern
#' @keywords internal
#' @export
#' @examples
#' lanePattern
lanePattern <- "_L([[:digit:]]{3})"

# List item (following HTML convention).
li <- "  -"

# NA strings.
na <- c("", "NA", "#N/A", "NULL", "null")

# Ignore case.
rdataExtPattern <- "\\.(rd[a|ata|s])$"

rdataError <- paste(
    "Failed to load data.",
    "R data files must contain `.rda`, `.rds`, or `.RData` extension.",
    sep = "\n"
)

#' Update Message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run `updateObject()` to update your object."
