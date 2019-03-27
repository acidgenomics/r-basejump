globalVariables(".")



packageVersion <- packageVersion("basejump")

#' basejump test data URL
#' @keywords internal
#' @export
#' @examples
#' basejumpTestsURL
basejumpTestsURL <- pasteURL(
    "tests.acidgenomics.com",
    "basejump",
    paste0("v", packageVersion$major, ".", packageVersion$minor),  # nolint
    protocol = "http"
)



#' Single-sell barcode pattern
#'
#' Trailing number is to match cellranger output.
#'
#' @export
#' @examples
#' barcodePattern
barcodePattern <- ")_([ACGT_]{6,})(_[0-9]+)?$"



#' Shared list of optional default formals
#'
#' @export
#' @examples
#' formalsList
formalsList <- list(
    color.continuous = quote(
        getOption("basejump.color.continuous", default = NULL)
    ),
    color.discrete = quote(
        getOption("basejump.color.discrete", default = NULL)
    ),
    fill.continuous = quote(
        getOption("basejump.fill.continuous", default = NULL)
    ),
    fill.discrete = quote(
        getOption("basejump.fill.discrete", default = NULL)
    ),
    flip = quote(
        getOption("basejump.flip", default = TRUE)
    ),
    label = quote(
        getOption("basejump.label", default = FALSE)
    ),
    legend = quote(
        getOption("basejump.legend", default = TRUE)
    ),
    point.size = quote(
        getOption("basejump.point.size", default = 3L)
    )
)



#' Slot names in metadata containing genome information
#' @export
#' @examples
#' genomeMetadataNames
genomeMetadataNames <- c("organism", "genomeBuild", "ensemblRelease")



#' Sequencing lane grep pattern
#' @export
#' @examples
#' lanePattern
lanePattern <- "_L([[:digit:]]{3})"



#' Sample metadata blacklist
#' @export
#' @examples
#' metadataBlacklist
metadataBlacklist <- c(
    # Too vague.
    "ID", "Id", "id",
    # Generated automatically.
    "interestingGroups",
    # Use "sampleName" instead.
    "name",
    # Generated automatically from "sequence" column.
    "revcomp",
    # Used internally by dplyr.
    "rowname",
    # Use "sampleName" instead.
    "sample",
    # "sampleID" is set automatically, for multiplexed/cell-level data.
    "sampleID", "sampleId", "sampleid",
    # Use "sampleName" instead.
    "samplename"
)



#' Update message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run updateObject() to update your object."
