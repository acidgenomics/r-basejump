#' Sample Metadata Constructor
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr arrange everything mutate mutate_if select
#' @importFrom magrittr set_rownames
#' @importFrom rlang .data syms !!!
#'
#' @param metadata Metadata [data.frame].
#'
#' @return [data.frame].
.prepareSampleMetadata <- function(metadata) {
    metadata <- as.data.frame(metadata)
    # `description` is required
    if (!"description" %in% colnames(metadata)) {
        stop("'description' column is required", .call = FALSE)
    }
    # Set `sampleName`, if necessary
    if (!"sampleName" %in% colnames(metadata)) {
        metadata[["sampleName"]] <- metadata[["description"]]
    }
    # Set `sampleID`, if necessary
    if (!"sampleID" %in% colnames(metadata)) {
        metadata[["sampleID"]] <- metadata[["sampleName"]]
    }
    # Ensure `sampleID` has valid names. This allows for input of samples
    # beginning with numbers or containing hyphens for example, which aren't
    # valid names in R.
    metadata[["sampleID"]] <- gsub(
            x = make.names(metadata[["sampleID"]], unique = TRUE),
            pattern = "\\.",
            replacement = "_")
    metadata %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, droplevels) %>%
        select(metadataPriorityCols, everything()) %>%
        arrange(!!!syms(metadataPriorityCols)) %>%
        set_rownames(.[["sampleID"]])
}
