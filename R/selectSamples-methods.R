# FIXME Need to add on support for `filterCells` in bcbioSingleCell.
# `prefilter = TRUE` argument



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
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @return `SingleCellExperiment`.
#'
#' @seealso
#' - [sampleData()].
#' - [S4Vectors::split()].
#'
#' @examples
#' object <- sce_small
#' sample <- sampleNames(object) %>% head(1L)
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
NULL



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
            stop("Arguments must be atomic vectors")
        }

        # Match the arguments against the sample metadata.
        sampleData <- as_tibble(sampleData(object), rownames = "sampleID")

        matches <- mapply(
            col = names(args),
            arg = args,
            function(col, arg) {
                # Check that column is present.
                if (!col %in% colnames(sampleData)) {
                    stop(paste(col, "isn't present in metadata colnames"))
                }
                # Check that all items in argument are present.
                if (!all(arg %in% sampleData[[col]])) {
                    missing <- arg[which(!arg %in% sampleData[[col]])]
                    stop(paste(
                        deparse(col),
                        "metadata column doesn't contain",
                        toString(missing)
                    ))
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
        message(paste(length(cells), "cells matched"))

        object <- object[, cells]
        metadata(object)[["selectSamples"]] <- TRUE
        object
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SingleCellExperiment"),
    definition = .selectSamples.SCE
)
