#' Select Samples
#'
#' Utility function that enables quick an easy sample selection in Bioconductor
#' contains that don't correspond to samples in the columns
#' (e.g. `SingleCellExperiment`).
#'
#' @details Internally, pattern matching against sample and file names is
#'   applied using logical grep matching.
#'
#' @note Bracket based subsetting with `[` also works on `SingleCellExperiment`
#'   objects but it's not intuitive. In this case, provide cellular barcode
#'   identifiers for columns and gene identifiers for rows.
#'
#' @name selectSamples
#' @family SummarizedExperiment Functions
#' @export
#'
#' @inheritParams general
#' @param ... Selection arguments that map to the column names of
#'   [sampleData()]. `atomic` values are supported. Avoid using `logical` or
#'   `numeric` indices (e.g. [base::which()] calls) here, for improved code
#'   readability.
#'
#' @return Modified object, with selected samples.
#'
#' @seealso
#' - [sampleData()].
#' - [S4Vectors::split()].
#'
#' @examples
#' data(rse_small, sce_small)
#'
#' # SummarizedExperiment ====
#' object <- rse_small
#' sample <- sampleNames(object) %>% head(1L)
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
#'
#' # SingleCellExperiment ====
#' object <- sce_small
#' sample <- sampleNames(object) %>% head(1L)
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
NULL



.selectSamples.SE <-  # nolint
    function(object, ...) {
        validObject(object)
        args <- list(...)
        invisible(lapply(args, assert_is_atomic))

        # Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        assert_is_subset(names(args), colnames(sampleData))

        # Obtain the sample identifiers.
        list <- mapply(
            col = names(args),
            arg = args,
            MoreArgs = list(data = sampleData),
            FUN = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        samples <- sort(as.character(Reduce(f = intersect, x = list)))
        assert_is_non_empty(samples)

        object[, samples]
    }



.selectSamples.SCE <-  # nolint
    function(object, ...) {
        validObject(object)

        # Here the `args` are captured as a named character vector. The
        # names of the arguments represent the column names. The value of the
        # arguments should be a string that can be used for logical grep
        # matching here internally.
        args <- list(...)
        if (!all(vapply(
            X = args,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        ))) {
            stop("Arguments must be atomic.")
        }

        # Match the arguments against the sample metadata.
        sampleData <- as_tibble(sampleData(object), rownames = "sampleID")

        matches <- mapply(
            col = names(args),
            arg = args,
            function(col, arg) {
                # Check that column is present.
                if (!col %in% colnames(sampleData)) {
                    stop(paste(
                        col, "isn't present in metadata colnames."
                    ), call. = FALSE)
                }
                # Check that all items in argument are present.
                if (!all(arg %in% sampleData[[col]])) {
                    missing <- arg[which(!arg %in% sampleData[[col]])]
                    stop(paste(
                        deparse(col),
                        "metadata column doesn't contain:",
                        toString(missing)
                    ), call. = FALSE)
                }
                # Get the sample ID matches.
                sampleData %>%
                    filter(!!sym(col) %in% !!arg) %>%
                    pull("sampleID") %>%
                    as.character()
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        samples <- Reduce(f = intersect, x = matches)
        assert_is_non_empty(samples)

        # Output to the user which samples matched, using the `sampleName`
        # metadata column, which is more descriptive than `sampleID`
        sampleNames <- sampleData %>%
            filter(!!sym("sampleID") %in% !!samples) %>%
            pull("sampleName") %>%
            as.character() %>%
            sort() %>%
            unique()

        message(paste(
            length(sampleNames), "sample(s) matched:",
            toString(sampleNames)
        ))

        # Use the metrics `data.frame` to match the cellular barcodes
        metrics <- metrics(object)
        assert_is_all_of(metrics, "grouped_df")
        assert_is_subset(c("cellID", "sampleID"), colnames(metrics))
        # Note that we don't need to sort here.
        cells <- metrics %>%
            filter(!!sym("sampleID") %in% !!samples) %>%
            pull("cellID")
        message(paste(length(cells), "cells matched."))

        object <- object[, cells]
        metadata(object)[["selectSamples"]] <- TRUE
        object
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SummarizedExperiment"),
    definition = .selectSamples.SE
)



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SingleCellExperiment"),
    definition = .selectSamples.SCE
)
