#' Sample YAML Metadata Utilities
#'
#' @rdname sampleYAML
#' @name sampleYAML
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param object Project summary YAML list.
#' @param ... Nested operator keys supplied as dot objects.
#'
#' @note Metrics are only generated for a standard RNA-seq run with aligned
#'   counts. Fast RNA-seq mode with lightweight counts (pseudocounts) doesn't
#'   output the same metrics into the YAML.
#'
#' @return [tibble].
NULL



# Constructors ====
.sampleYAML <- function(object, ...) {
    samples <- object[["samples"]]
    if (!length(samples)) {
        stop("No sample information in YAML")
    }

    # Check for nested keys, otherwise return NULL
    # Improve recursion method in a future update (lower priority)
    keys <- dots(..., character = TRUE)
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
        # List can be coerced to data frame using [data.table::rbindlist()] or
        # [dplyr::bind_rows()]. Some YAML files will cause [bind_rows()] to
        # throw `Column XXX can't be converted from integer to character` errors
        # on numeric data, whereas this doesn't happen with [rbindlist()].
        rbindlist(fill = TRUE) %>%
        as("tibble") %>%
        camel(strict = FALSE) %>%
        removeNA() %>%
        mutate(
            # Copy `description` to `sampleName`
            sampleName = .data[["description"]],
            # Sanitize `sampleID` into valid names
            sampleID = make.names(
                str_replace_all(.data[["sampleID"]], "-", "_")
            )
        )
}



# Methods ====
#' @rdname sampleYAML
#' @export
setMethod(
    "sampleYAML",
    signature = "list",
    definition = .sampleYAML)
