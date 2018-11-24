globalVariables(".")

packageVersion <- packageVersion("basejump")

#' Basejump Cache URL
#' @keywords internal
#' @export
#' @examples
#' basejumpCacheURL
basejumpCacheURL <- paste0(
    "http://basejump.seq.cloud/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
)

#' Single-Cell Barcode Pattern
#' Trailing number is to match cellranger output.
#' @export
#' @examples
#' barcodePattern
barcodePattern <- ")_([ACGT_]{6,})(_[0-9]+)?$"

#' File Extension Pattern
#' Note optional matching of gzip.
#' @export
#' @examples
#' extPattern
extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"

#' Formals List
#' Formals that the user can set globally with `base::getOption()`.
#' @export
#' @examples
#' formalsList
formalsList <- list(
    color.continuous = quote(getOption("basejump.color.continuous", NULL)),
    color.discrete = quote(getOption("basejump.color.discrete", NULL)),
    data.frame = quote(getOption("basejump.data.frame", "data.frame")),
    fill.continuous = quote(getOption("basejump.fill.continuous", NULL)),
    fill.discrete = quote(getOption("basejump.fill.discrete", NULL)),
    flip = quote(getOption("basejump.flip", TRUE)),
    label = quote(getOption("basejump.label", FALSE)),
    legend = quote(getOption("basejump.legend", TRUE)),
    load.dir = quote(getOption("basejump.load.dir", ".")),
    point.size = quote(getOption("basejump.point.size", 3L)),
    save.dir = quote(getOption("basejump.save.dir", ".")),
    save.ext = quote(getOption("basejump.save.ext", "rds")),
    save.overwrite = quote(getOption("basejump.save.overwrite", TRUE)),
    save.compress = quote(getOption("basejump.save.compress", TRUE))
)

#' Slot Names in Metadata Containing Genome Information
#' @export
#' @examples
#' genomeMetadataNames
genomeMetadataNames <- c("organism", "genomeBuild", "ensemblRelease")

#' Sequencing Lane Grep Pattern
#' @export
#' @examples
#' lanePattern
lanePattern <- "_L([[:digit:]]{3})"

#' NA Strings
#' @export
#' @examples
#' naStrings
naStrings <- c("", "NA", "#N/A", "NULL", "null")

#' R Data Load Error
#' @export
#' @examples
#' message(rdataLoadError)
rdataLoadError <- paste(
    "Failed to load data.",
    "R data files must contain `.rda`, `.rds`, or `.RData` extension.",
    sep = "\n"
)

#' R Data Extension Pattern
#' @export
#' @examples
#' rdataExtPattern
rdataExtPattern <- "\\.(rd[a|ata|s])$"

#' Update Message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run `updateObject()` to update your object."

.dataFrameChoices <- c("data.frame", "DataFrame", "tbl_df", "data.table")
