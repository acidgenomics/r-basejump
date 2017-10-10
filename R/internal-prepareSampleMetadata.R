#' Sample Metadata Constructor
#'
#' @keywords internal
#' @noRd
#'
#' @param metadata Metadata [data.frame].
#'
#' @return [data.frame].
.prepareSampleMetadata <- function(metadata) {
    metadata %>%
        as.data.frame() %>%
        # Ensure `sampleID` has valid names. This allows for input of samples
        # beginning with numbers or containing hyphens for example, which aren't
        # valid names in R.
        mutate(
            sampleID = make.names(
                str_replace_all(.data[["sampleID"]], "-", "_"))
        ) %>%
        # Set all non-priority columns as factor
        mutate_if(!colnames(.) %in% metadataPriorityCols, factor) %>%
        # Put the priority columns first and arrange rows
        dplyr::select(metadataPriorityCols, everything()) %>%
        arrange(!!!syms(metadataPriorityCols)) %>%
        set_rownames(.[["sampleID"]])
}
