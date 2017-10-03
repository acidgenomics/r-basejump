#' Sample Metrics from YAML File
#'
#' @family Exported Constructor Functions
#' @keywords internal
#'
#' @param yaml YAML [list].
#' @param characterCols Columns to set as character.
#'
#' @return [data.frame].
#' @export
.sampleYAMLMetrics <- function(
    yaml,
    characterCols) {
    metrics <- .sampleYAML(yaml, summary, metrics)
    if (is.null(metrics)) {
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
