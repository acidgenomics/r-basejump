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
#' @return [tibble].
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
#' @importFrom data.table rbindlist
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

    lapply(seq_along(samples), function(a) {
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
            camel(strict = FALSE) %>%
            .[unique(names(.))]
    }) %>%
        # Some YAML files will cause `dplyr::bind_rows()` to throw
        # `Column XXX can't be converted from integer to character` errors
        # on numeric data, whereas this doesn't happen with `rbindlist()`.
        rbindlist(fill = TRUE) %>%
        as("tibble") %>%
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
