#' Sample Metrics from YAML File
#'
#' @rdname sampleYAMLMetrics
#' @name sampleYAMLMetrics
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param yaml YAML [list].
#'
#' @return [data.frame].
NULL



# Constructors ====
.sampleYAMLMetrics <- function(yaml) {
    # Here `summary` and `metrics` are keys passed in as symbols
    metrics <- sampleYAML(yaml, summary, metrics)
    # The fast mode RNA-seq pipeline doesn't report metrics generated from
    # STAR featureCounts output with MultiQC. Allow NULL return to handle
    # this pipeline output.
    if (is.null(metrics)) {
        warning("No sample metrics were calculated", call. = FALSE)
        return(NULL)
    }
    # Fix numerics set as characters
    numericAsCharacter <- function(x) {
        all(str_detect(x, "^[0-9\\.]+$"))
    }
    mutate_if(metrics, numericAsCharacter, as.numeric)
}



# Methods ====
#' @rdname sampleYAMLMetrics
#' @export
setMethod(
    "sampleYAMLMetrics",
    signature = "list",
    definition = .sampleYAMLMetrics)
