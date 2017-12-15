#' Sample Metrics from YAML File
#'
#' @rdname sampleYAMLMetrics
#' @name sampleYAMLMetrics
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
#' sampleYAMLMetrics(yaml) %>% glimpse()
NULL



# Constructors =================================================================
#' @importFrom dplyr mutate_if
.sampleYAMLMetrics <- function(yaml) {
    metrics <- sampleYAML(
        yaml = yaml,
        keys = c("summary", "metrics")
    )
    # The fast mode RNA-seq pipeline doesn't report metrics generated from
    # STAR featureCounts output with MultiQC. Allow NULL return to handle
    # this pipeline output.
    if (is.null(metrics)) {
        warning("No sample metrics were calculated", call. = FALSE)
        return(NULL)
    }
    # Fix numerics set as characters
    numericAsCharacter <- function(x) {
        any(grepl(x = x, pattern = "^[0-9\\.]+$"))
    }
    metrics %>%
        mutate_if(is.factor, as.character) %>% 
        mutate_if(numericAsCharacter, as.numeric) %>%
        mutate_if(is.character, as.factor) %>%
        .prepareSampleMetadata()
}



# Methods ======================================================================
#' @rdname sampleYAMLMetrics
#' @export
setMethod(
    "sampleYAMLMetrics",
    signature("list"),
    .sampleYAMLMetrics)
