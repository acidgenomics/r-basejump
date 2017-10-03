#' Sample Metadata from YAML
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @param yaml YAML [list].
#'
#' @return [data.frame].
#' @export
.sampleYAMLMetadata <- function(yaml) {
    .sampleYAML(yaml, metadata) %>%
        .setMetadataFactors()
}
