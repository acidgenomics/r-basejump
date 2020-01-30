## FIXME Rename generic to x.



#' @rdname aggregate
#' @name aggregateCols
#' @importFrom acidgenerics aggregateCols
#' @usage aggregateCols(object, ...)
#' @export
NULL



## Updated 2020-01-30.
`aggregateCols,matrix` <-  # nolint
    function(
        object,
        by,
        FUN  # nolint
    ) {
        FUN <- match.arg(FUN)
        object <- t(object)
        object <- aggregateRows(object = object, by = by, FUN = FUN)
        object <- t(object)
        object
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = `aggregateCols,matrix`
)



## Updated 2020-01-30.
`aggregateCols,sparseMatrix` <-  # nolint
    function(
        object,
        by,
        FUN
    ) {
        FUN <- match.fun(FUN)
        object <- Matrix::t(object)
        object <- aggregateRows(object = object, by = by, FUN = FUN)
        object <- Matrix::t(object)
        object
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("sparseMatrix"),
    definition = `aggregateCols,sparseMatrix`
)



## Updated 2019-07-22.
`aggregateCols,SummarizedExperiment` <-  # nolint
    function(
        object,
        col = "aggregate",
        FUN
    ) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        FUN <- match.fun(FUN)

        ## Groupings -----------------------------------------------------------
        if (!all(
            isSubset(col, colnames(colData(object))),
            isSubset(col, colnames(sampleData(object)))
        )) {
            stop(sprintf(
                "'%s' column not defined in 'colData()'.", deparse(col)
            ))
        }
        by <- colData(object)[[col]]
        assert(
            is.factor(by),
            validNames(levels(by)),
            identical(length(by), ncol(object))
        )
        names(by) <- colnames(object)

        ## Counts --------------------------------------------------------------
        counts <- aggregateCols(object = counts(object), by = by, FUN = FUN)
        assert(identical(nrow(counts), nrow(object)))

        ## Return --------------------------------------------------------------
        args <- list(
            assays = SimpleList(counts = counts),
            colData = DataFrame(row.names = colnames(counts))
        )
        if (is(object, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- rowRanges(object)
        } else {
            args[["rowData"]] <- rowData(object)
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }

formals(`aggregateCols,SummarizedExperiment`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateCols,SummarizedExperiment`
)



## Updated 2020-01-30.
`aggregateCols,SingleCellExperiment` <-  # nolint
    function(
        object,
        FUN  # nolint
    ) {
        validObject(object)
        FUN <- match.fun(FUN)
        ## Remap cellular barcodes.
        colData <- colData(object)
        assert(
            isSubset(c("sampleID", "aggregate"), colnames(colData)),
            is.factor(colData[["aggregate"]])
        )
        cli_alert(sprintf(
            "Remapping cells to aggregate samples: %s",
            toString(sort(levels(colData[["aggregate"]])), width = 100L)
        ))
        map <- colData(object)
        map <- as_tibble(map, rownames = "cellID")
        ## Check to see if we can aggregate.
        if (!all(mapply(
            FUN = grepl,
            x = map[["cellID"]],
            pattern = paste0("^", map[["sampleID"]]),
            SIMPLIFY = TRUE
        ))) {
            stop("Cell IDs are not prefixed with sample IDs.")
        }
        by <- mapply(
            FUN = gsub,
            x = map[["cellID"]],
            pattern = paste0("^", map[["sampleID"]]),
            replacement = map[["aggregate"]],
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
        by <- as.factor(by)
        cell2sample <- as.factor(map[["aggregate"]])
        names(cell2sample) <- as.character(by)
        ## Reslot the `aggregate` column using these groupings.
        assert(identical(names(by), colnames(object)))
        colData(object)[["aggregate"]] <- by

        ## Generate SingleCellExperiment ---------------------------------------
        ## Using `SummarizedExperiment` method here.
        rse <- as(object, "RangedSummarizedExperiment")
        colData(rse)[["sampleID"]] <- NULL
        rse <- aggregateCols(object = rse, FUN = FUN)
        assert(
            is(rse, "RangedSummarizedExperiment"),
            identical(nrow(rse), nrow(object))
        )
        ## Update the sample data.
        colData <- colData(rse)
        assert(isSubset(rownames(colData), names(cell2sample)))
        colData[["sampleID"]] <- cell2sample[rownames(colData)]
        colData[["sampleName"]] <- colData[["sampleID"]]
        colData(rse) <- colData
        ## Update the metadata.
        metadata <- metadata(object)
        metadata[["aggregate"]] <- TRUE
        metadata[["aggregateCols"]] <- by
        ## Now ready to generate aggregated SCE.
        sce <- SingleCellExperiment(
            assays = SimpleList(counts = counts(rse)),
            rowRanges = rowRanges(object),
            colData = colData(rse),
            metadata = list(
                aggregate = TRUE,
                aggregateCols = by,
                interestingGroups = interestingGroups(object)
            )
        )
        validObject(sce)
        sce
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCols,SingleCellExperiment`
)
