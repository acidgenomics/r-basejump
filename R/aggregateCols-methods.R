#' @rdname aggregate
#' @name aggregateCols
#' @importFrom acidgenerics aggregateCols
#' @usage aggregateCols(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`aggregateCols,matrix` <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        fun <- match.arg(fun)
        object <- t(object)
        object <- aggregateRows(
            object = object,
            groupings = groupings,
            fun = fun
        )
        assert(is.matrix(object))
        object <- t(object)
        object
    }

formals(`aggregateCols,matrix`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = `aggregateCols,matrix`
)



## Updated 2019-07-22.
`aggregateCols,sparseMatrix` <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        fun <- match.arg(fun)
        object <- Matrix::t(object)
        object <- aggregateRows(
            object = object,
            groupings = groupings,
            fun = fun
        )
        assert(is(object, "Matrix"))
        object <- Matrix::t(object)
        object
    }

formals(`aggregateCols,sparseMatrix`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("sparseMatrix"),
    definition = `aggregateCols,sparseMatrix`
)



## Updated 2019-07-22.
`aggregateCols,SummarizedExperiment` <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        fun <- match.arg(fun)

        ## Groupings -----------------------------------------------------------
        if (!all(
            isSubset(col, colnames(colData(object))),
            isSubset(col, colnames(sampleData(object)))
        )) {
            stop(sprintf(
                "'%s' column not defined in 'colData()'.", deparse(col)
            ))
        }
        groupings <- colData(object)[[col]]
        assert(
            is.factor(groupings),
            validNames(levels(groupings)),
            identical(length(groupings), ncol(object))
        )
        names(groupings) <- colnames(object)

        ## Counts --------------------------------------------------------------
        counts <- aggregateCols(
            object = counts(object),
            groupings = groupings,
            fun = fun
        )
        assert(identical(nrow(counts), nrow(object)))
        if (fun == "sum") {
            assert(identical(sum(counts), sum(counts(object))))
        }

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



## Updated 2020-01-20.
`aggregateCols,SingleCellExperiment` <-  # nolint
    function(object, fun) {
        validObject(object)
        fun <- match.arg(fun)
        ## Remap cellular barcode groupings.
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
        groupings <- mapply(
            FUN = gsub,
            x = map[["cellID"]],
            pattern = paste0("^", map[["sampleID"]]),
            replacement = map[["aggregate"]],
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
        groupings <- as.factor(groupings)
        cell2sample <- as.factor(map[["aggregate"]])
        names(cell2sample) <- as.character(groupings)
        ## Reslot the `aggregate` column using these groupings.
        assert(identical(names(groupings), colnames(object)))
        colData(object)[["aggregate"]] <- groupings

        ## Generate SingleCellExperiment ---------------------------------------
        ## Using `SummarizedExperiment` method here.
        rse <- as(object, "RangedSummarizedExperiment")
        colData(rse)[["sampleID"]] <- NULL
        rse <- aggregateCols(object = rse, fun = fun)
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
        metadata[["aggregateCols"]] <- groupings
        ## Now ready to generate aggregated SCE.
        sce <- SingleCellExperiment(
            assays = SimpleList(counts = counts(rse)),
            rowRanges = rowRanges(object),
            colData = colData(rse),
            metadata = list(
                aggregate = TRUE,
                aggregateCols = groupings,
                interestingGroups = interestingGroups(object)
            )
        )
        validObject(sce)
        sce
    }

formals(`aggregateCols,SingleCellExperiment`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCols,SingleCellExperiment`
)
