#' Set Metadata Columns as Factors
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @param meta Metadata [data.frame].
#'
#' @return [data.frame].
#' @export
.setMetadataFactors <- function(meta) {
    meta %>%
        mutate_if(!colnames(.) %in% metaPriorityCols, factor)
}
