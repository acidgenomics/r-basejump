#' @name selectSamples
#' @inherit AcidGenerics::selectSamples
#' @note Updated 2020-01-20.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
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



## Updated 2019-07-22.
`selectSamples,SummarizedExperiment` <-  # nolint
    function(object, ...) {
        validObject(object)
        args <- list(...)
        ## Check that all arguments are atomic.
        if (!all(vapply(
            X = args,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        ))) {
            stop("Arguments must be atomic.")  # nocov
        }
        ## Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        assert(isSubset(names(args), colnames(sampleData)))
        ## Obtain the sample identifiers.
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
        ## Return.
        object[, samples, drop = FALSE]
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SummarizedExperiment"),
    definition = `selectSamples,SummarizedExperiment`
)



## Updated 2019-08-19.
`selectSamples,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        validObject(object)
        assert(isSubset("sampleId", colnames(colData(object))))
        ## Here the `args` are captured as a named character vector. The names
        ## of the arguments represent the column names. The value of the
        ## arguments should be a string that can be used for logical grep
        ## matching here internally.
        args <- list(...)
        if (!all(vapply(
            X = args,
            FUN = is.atomic,
            FUN.VALUE = logical(1L)
        ))) {
            stop("Arguments must be atomic.")
        }
        ## Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        ## Allowing the user to select by "sampleId".
        sampleData[["sampleId"]] <- rownames(sampleData)
        assert(hasRownames(sampleData))
        matches <- mapply(
            col = names(args),
            arg = args,
            function(col, arg) {
                ## Check that column is present.
                if (!col %in% colnames(sampleData)) {
                    ## nocov start
                    stop(sprintf(
                        "'%s' isn't present in 'sampleData()'.", col
                    ))
                    ## nocov end
                }
                ## Check that all items in argument are present.
                if (!all(arg %in% sampleData[[col]])) {
                    ## nocov start
                    missing <- arg[which(!arg %in% sampleData[[col]])]
                    stop(sprintf(
                        "'%s' metadata column doesn't contain: %s.",
                        col, toString(missing, width = 100L)
                    ))
                    ## nocov end
                }
                ## Get the sample ID matches.
                keep <- sampleData[[col]] %in% arg
                data <- sampleData[keep, , drop = FALSE]
                rownames(data)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        samples <- Reduce(f = intersect, x = matches)
        assert(hasLength(samples))
        ## Output to the user which samples matched, using the `sampleName`
        ## metadata column, which is more descriptive than `sampleId`
        sampleNames <- sampleData[samples, "sampleName", drop = TRUE]
        sampleNames <- sort(unique(as.character(sampleNames)))
        cli_alert_info(sprintf(
            "%d %s matched: %s.",
            length(sampleNames),
            ngettext(
                n = length(sampleNames),
                msg1 = "sample",
                msg2 = "samples"
            ),
            toString(sampleNames, width = 100L)
        ))

        colData <- colData(object)
        keep <- colData[["sampleId"]] %in% samples
        colData <- colData[keep, , drop = FALSE]
        cells <- rownames(colData)
        cli_alert_info(sprintf(
            "%d %s matched.",
            length(cells),
            ngettext(
                n = length(cells),
                msg1 = "cell",
                msg2 = "cells"
            )
        ))
        object <- object[, cells, drop = FALSE]
        metadata(object)[["selectSamples"]] <- TRUE
        object
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SingleCellExperiment"),
    definition = `selectSamples,SingleCellExperiment`
)
