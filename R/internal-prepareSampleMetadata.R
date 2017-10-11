#' Sample Metadata Constructor
#'
#' @keywords internal
#' @noRd
#'
#' @param metadata Metadata [data.frame].
#' @param factors Set columns that don't apply to sample names as factors.
#'
#' @return [data.frame].
.prepareSampleMetadata <- function(metadata, factors = TRUE) {
    meta <- metadata %>%
        as.data.frame() %>%
        # Ensure `sampleID` has valid names. This allows for input of samples
        # beginning with numbers or containing hyphens for example, which aren't
        # valid names in R.
        mutate(
            sampleName = .data[["description"]],
            sampleID = make.names(
                str_replace_all(.data[["sampleName"]], "-", "_"))
        )
    if (isTRUE(factors)) {
        # Set all non-priority columns as factor
        meta <- meta %>%
            mutate_if(!colnames(.) %in% metadataPriorityCols, factor)
    }
    meta %>%
        # Put the priority columns first and arrange rows
        dplyr::select(metadataPriorityCols, everything()) %>%
        arrange(!!!syms(metadataPriorityCols)) %>%
        set_rownames(.[["sampleID"]])
}
