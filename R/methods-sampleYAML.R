#' Sample YAML Metadata Utilities
#'
#' @rdname sampleYAML
#' @name sampleYAML
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param yaml Project summary YAML list.
#' @param keys Nested operator keys, supplied as a character vector.
#'
#' @note Metrics are only generated for a standard RNA-seq run with aligned
#'   counts. Fast RNA-seq mode with lightweight counts (pseudocounts) doesn't
#'   output the same metrics into the YAML.
#'
#' @return [data.frame].
#'
#' @examples
#' url <- file.path(
#'     "http://basejump.seq.cloud",
#'     "bcbio",
#'     "project-summary.yaml")
#' yaml <- readYAML(url)
#' sampleYAML(yaml, "metadata")
NULL



# Constructors ====
.sampleYAML <- function(yaml, keys) {
    samples <- yaml[["samples"]]
    if (!length(samples)) {
        stop("No sample information in YAML", call. = FALSE)
    }

    if (!keys[[1]] %in% names(samples[[1]])) {
        return(NULL)
    }
    if (length(keys) > 1) {
        if (!keys[[2]] %in% names(samples[[1]][[keys[[1]]]])) {
            return(NULL)
        }
    }

    data <- lapply(seq_along(samples), function(a) {
        nested <- samples[[a]][[keys]]
        # Set the description
        nested[["description"]] <- samples[[a]][["description"]]
        if (rev(keys)[[1]] == "metadata") {
            if (is.null(nested[["batch"]])) {
                nested[["batch"]] <- NA
            }
            if (length(nested[["phenotype"]])) {
                if (grepl("^$", nested[["phenotype"]])) {
                    nested[["phenotype"]] <- NA
                }
            }
        }
        nested %>%
            .[unique(names(.))] %>%
            unlist()
    })

    data %>%
        as.data.frame.list() %>%
        t() %>%
        as.data.frame() %>%
        set_rownames(NULL) %>%
        camel(strict = FALSE) %>%
        removeNA()
}



# Methods ====
#' @rdname sampleYAML
#' @export
setMethod(
    "sampleYAML",
    signature(yaml = "list",
              keys = "character"),
    .sampleYAML)
