#' Arrange Metadata by Priority Columns
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @param meta Sample metadata [data.frame].
#'
#' @return [data.frame].
#' @export
.arrangeMetadataByPriorityCols <- function(meta) {
    if (!exists(metaPriorityCols)) {
        stop("'metaPriorityCols' character vector missing from environment")
    }
    meta %>%
        as("tibble") %>%
        # Sanitize `sampleID` into valid names
        mutate(sampleID = make.names(.data[["sampleName"]])) %>%
        .[, unique(c(metaPriorityCols, sort(colnames(.))))] %>%
        arrange(!!!syms(metaPriorityCols))
}
