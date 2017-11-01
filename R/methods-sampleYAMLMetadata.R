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
#' url <- file.path(
#'     "http://basejump.seq.cloud",
#'     "bcbio",
#'     "project-summary.yaml")
#' yaml <- readYAML(url)
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
