#' Sample Metadata
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @param meta Metadata [data.frame].
#'
#' @return [data.frame].
#' @export
.sampleMetadata <- function(meta) {
    meta %>%
        as.data.frame() %>%
        # Ensure `sampleID` has valid names:
        # - `1234_sample` = `X1234_sample`
        # - `sample_1-ACGTACGT` = `sample_1_ACGTACGT`
        # This allows for input of samples beginning with numbers or containing
        # hyphens for example, which aren't valid names in R
        mutate(
            sampleID = make.names(
                str_replace_all(.data[["sampleID"]], "-", "_"))
        ) %>%
        # Set all non-priority columns as factor
        mutate_if(!colnames(.) %in% metaPriorityCols, factor) %>%
        # Put the priority columns first and arrange rows
        dplyr::select(metaPriorityCols, everything()) %>%
        arrange(!!!syms(metaPriorityCols)) %>%
        set_rownames(.[["sampleID"]])
}
