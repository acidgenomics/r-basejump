globalVariables(".")



packageVersion <- packageVersion("basejump")

#' basejump package cache URL
#' @keywords internal
#' @export
#' @examples
#' basejumpCacheURL
basejumpCacheURL <- paste0(
    "http://basejump.seq.cloud/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
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
    color.continuous = quote(getOption("basejump.color.continuous", NULL)),
    color.discrete = quote(getOption("basejump.color.discrete", NULL)),
    fill.continuous = quote(getOption("basejump.fill.continuous", NULL)),
    fill.discrete = quote(getOption("basejump.fill.discrete", NULL)),
    flip = quote(getOption("basejump.flip", TRUE)),
    label = quote(getOption("basejump.label", FALSE)),
    legend = quote(getOption("basejump.legend", TRUE)),
    point.size = quote(getOption("basejump.point.size", 3L))
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



#' Update message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run updateObject() to update your object."
