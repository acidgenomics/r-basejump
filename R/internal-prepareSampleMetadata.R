#' Sample Metadata Constructor
#'
#' @keywords internal
#'
#' @importFrom dplyr arrange everything mutate mutate_if select
#' @importFrom magrittr set_rownames
#' @importFrom rlang .data syms !!!
#'
#' @param metadata Metadata [data.frame].
#' @param factors Set columns that don't apply to sample names as factors
#'   (`TRUE`/`FALSE`).
#'
#' @return [data.frame].
.prepareSampleMetadata <- function(metadata, factors = TRUE) {
    metadata <- as.data.frame(metadata)
    # `description` must already be defined
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
    if (isTRUE(factors)) {
        # Set all non-priority columns as factor
        metadata <- metadata %>%
            mutate_if(!colnames(.) %in%
                          c(metadataPriorityCols, "fileName"),
                      factor)
    }
    metadata %>%
        # Ensure `sampleID` has valid names. This allows for input of samples
        # beginning with numbers or containing hyphens for example, which aren't
        # valid names in R.
        mutate(sampleID = gsub(
            x = make.names(.data[["sampleID"]], unique = TRUE),
            pattern = "\\.",
            replacement = "_")
        ) %>%
        # Put the priority columns first and arrange the rows by them
        select(metadataPriorityCols, everything()) %>%
        arrange(!!!syms(metadataPriorityCols)) %>%
        set_rownames(.[["sampleID"]])
}
