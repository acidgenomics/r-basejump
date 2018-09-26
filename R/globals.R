globalVariables(".")

version <- packageVersion("basejump")
versionDir <- paste0("v", version$major, ".", version$minor)
# FIXME Migrate S3 to a bucket that supports https.
cacheURL <- paste0("http://basejump.seq.cloud/", versionDir)

# Trailing number is to match cellranger output.
barcodePattern <- ")_([ACGT_]{6,})(_[0-9]+)?$"

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
