#' Sample Metadata from YAML
#'
#' @rdname sampleYAMLMetadata
#' @name sampleYAMLMetadata
#' @family bcbio Utilities
#' @keywords internal
#'
#' @inherit sampleYAML
#'
#' @examples
#' yaml <- readYAML(file.path(testDataURL, "project-summary.yaml"))
#' sampleYAMLMetadata(yaml)
NULL



# Methods ====
#' @rdname sampleYAMLMetadata
#' @export
setMethod(
    "sampleYAMLMetadata",
    signature("list"),
    function(yaml) {
        sampleYAML(
            yaml = yaml,
            keys = "metadata"
        ) %>%
            .prepareSampleMetadata(factors = TRUE)
    })
