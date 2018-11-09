.dataFrameChoices <- c("data.frame", "DataFrame", "tbl_df", "data.table")



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
