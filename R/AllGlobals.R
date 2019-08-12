#' All global variables
#' @noRd
NULL



globalVariables(".")

.version <- packageVersion("basejump")

## Including these here for backward compatibility with R 3.5.
if (packageVersion("base") < "3.6") {
    appendToBody <- goalie::appendToBody
    methodFormals <- goalie::methodFormals
}



#' basejump test data URL
#' @keywords internal
#' @export
#' @examples
#' basejumpTestsURL
basejumpTestsURL <- paste0(
    "http://tests.acidgenomics.com/basejump/",
    "v", .version$major, ".", .version$minor  # nolint
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
        getOption(
            x = "acid.color.continuous",
            default = acidplots::scale_colour_synesthesia_c()
        )
    ),
    color.discrete = quote(
        getOption(
            x = "acid.color.discrete",
            default = acidplots::scale_colour_synesthesia_d()
        )
    ),
    fill.continuous = quote(
        getOption(
            x = "acid.fill.continuous",
            default = acidplots::scale_fill_synesthesia_c()
        )
    ),
    fill.discrete = quote(
        getOption(
            x = "acid.fill.discrete",
            default = acidplots::scale_fill_synesthesia_d()
        )
    ),
    flip = quote(
        getOption(x = "acid.flip", default = TRUE)
    ),
    heatmap.color = quote(
        getOption(
            x = "acid.heatmap.color",
            default = acidplots::synesthesia
        )
    ),
    label = quote(
        getOption(x = "acid.label", default = FALSE)
    ),
    legend = quote(
        getOption(x = "acid.legend", default = TRUE)
    ),
    point.size = quote(
        getOption(x = "acid.point.size", default = 3L)
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
#' @note `sampleID` is set automatically for multiplexed/cell-level data.
#' @export
#' @examples
#' metadataBlacklist
metadataBlacklist <- sort(c(
    ## Automatic / used internally
    "interestingGroups",
    "revcomp",
    "rowname",
    "sampleID",
    ## interestingGroups variants
    "interestinggroups",
    "intgroup",
    ## sampleID, sampleName variants
    "ID",
    "Id",
    "id",
    "name",
    "names",
    "sample",
    "samples",
    "sampleId",
    "sampleid",
    "sampleNames",
    "samplename",
    "samplenames"
))



#' Update message
#' @keywords internal
#' @export
#' @examples
#' message(updateMessage)
updateMessage <- "Run 'updateObject()' to update your object."
