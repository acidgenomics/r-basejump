#' Sample Metadata from YAML
#'
#' @rdname sampleYAMLMetadata
#' @name sampleYAMLMetadata
#' @family bcbio Utilities
#' @keywords internal
#'
#' @inherit sampleYAML
NULL



# Methods ====
#' @rdname sampleYAMLMetadata
#' @export
setMethod(
    "sampleYAMLMetadata",
    signature("list"),
    function(yaml) {
        # Here `metadata` is the key, passed in as a symbol
        sampleYAML(yaml, metadata) %>%
            .prepareSampleMetadata(factors = TRUE)
    })
