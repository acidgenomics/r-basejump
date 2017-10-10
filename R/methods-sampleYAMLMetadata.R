#' Sample Metadata from YAML
#'
#' @rdname sampleYAMLMetadata
#' @name sampleYAMLMetadata
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param object YAML [list].
#'
#' @return [data.frame].
NULL



# Methods ====
#' @rdname sampleYAMLMetadata
#' @export
setMethod(
    "sampleYAMLMetadata",
    signature = "list",
    definition = function(object) {
        # Here `metadata` is the key, passed in as a symbol
        .sampleYAML(object, metadata) %>%
            .sampleMetadata()
    })
