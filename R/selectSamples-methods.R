#' @name selectSamples
#' @inherit bioverbs::selectSamples
#' @inheritParams params
#' @examples
#' data(rse, sce, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' object <- rse
#' sample <- sampleNames(object) %>% head(1L)
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
#'
#' ## SingleCellExperiment ====
#' object <- sce
#' sample <- sampleNames(object) %>% head(1L)
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
NULL



#' @importFrom bioverbs selectSamples
#' @aliases NULL
#' @export
bioverbs::selectSamples



selectSamples.SummarizedExperiment <-  # nolint
    function(object, ...) {
        validObject(object)
        args <- list(...)

        # Check that all arguments are atomic.
        if (!all(vapply(
            X = args,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        ))) {
            stop("Arguments must be atomic.")
        }

        # Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        assert(isSubset(names(args), colnames(sampleData)))

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
        assert(hasLength(samples))

        object[, samples, drop = FALSE]
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SummarizedExperiment"),
    definition = selectSamples.SummarizedExperiment
)



selectSamples.SingleCellExperiment <-  # nolint
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
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID")

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
        assert(hasLength(samples))

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
        assert(
            is(metrics, "grouped_df"),
            isSubset(c("cellID", "sampleID"), colnames(metrics))
        )

        # Note that we don't need to sort here.
        cells <- metrics %>%
            filter(!!sym("sampleID") %in% !!samples) %>%
            pull("cellID")
        message(paste(length(cells), "cells matched."))

        object <- object[, cells, drop = FALSE]
        metadata(object)[["selectSamples"]] <- TRUE

        object
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SingleCellExperiment"),
    definition = selectSamples.SingleCellExperiment
)
