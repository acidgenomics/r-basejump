#' Sample Metrics from YAML File
#'
#' @rdname sampleYAMLMetrics
#' @name sampleYAMLMetrics
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param yaml YAML [list].
#' @param characterCols Columns to set as character.
#'
#' @return [data.frame].
NULL



# Constructors ====
.sampleYAMLMetrics <- function(
    yaml,
    characterCols) {
    # Here `summary` and `metrics` are keys passed in as symbols
    metrics <- sampleYAML(yaml, summary, metrics)
    # The fast mode RNA-seq pipeline doesn't report metrics generated from
    # STAR featureCounts output with MultiQC. Allow NULL return to handle
    # this pipeline output.
    if (is.null(metrics)) {
        warning("No sample metrics were calculated", call. = FALSE)
        return(NULL)
    }
    chr <- metrics %>%
        .[, unique(c(metaPriorityCols, characterCols)), drop = FALSE]
    num <- metrics %>%
        .[, setdiff(colnames(metrics), colnames(chr)), drop = FALSE] %>%
        mutate_if(is.character, as.numeric)
    bind_cols(chr, num) %>%
        .[, unique(c(metaPriorityCols, sort(colnames(.)))), drop = FALSE]
}



# Methods ====
#' @rdname sampleYAMLMetrics
#' @export
setMethod(
    "sampleYAMLMetrics",
    signature = "list",
    definition = .sampleYAMLMetrics)
