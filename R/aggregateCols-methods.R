#' @rdname aggregate
#' @name aggregateCols
#' @importFrom acidgenerics aggregateCols
#' @usage aggregateCols(x, ...)
#' @export
NULL



## Updated 2020-01-30.
`aggregateCols,matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "median", "geometricMean")
    ) {
        fun <- match.arg(fun)
        x <- t(x)
        x <- aggregateRows(x = x, by = by, fun = fun)
        x <- t(x)
        x
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = `aggregateCols,matrix`
)



## Updated 2020-01-30.
`aggregateCols,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean")
    ) {
        fun <- match.arg(fun)
        x <- Matrix::t(x)
        x <- aggregateRows(x = x, by = by, fun = fun)
        x <- Matrix::t(x)
        x
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("Matrix"),
    definition = `aggregateCols,Matrix`
)



## Updated 2020-01-30.
`aggregateCols,SummarizedExperiment` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
        validObject(x)
        assert(
            hasValidDimnames(x),
            isString(col),
            isString(fun)
        )

        ## Groupings -----------------------------------------------------------
        if (!all(
            isSubset(col, colnames(colData(x))),
            isSubset(col, colnames(sampleData(x)))
        )) {
            stop(sprintf(
                "'%s' column not defined in 'colData()'.", deparse(col)
            ))
        }
        by <- colData(x)[[col]]
        assert(
            is.factor(by),
            validNames(levels(by)),
            identical(length(by), ncol(x))
        )
        names(by) <- colnames(x)

        ## Counts --------------------------------------------------------------
        counts <- aggregateCols(x = counts(x), by = by, fun = fun)
        assert(identical(nrow(counts), nrow(x)))

        ## Return --------------------------------------------------------------
        args <- list(
            assays = SimpleList(counts = counts),
            colData = DataFrame(row.names = colnames(counts))
        )
        if (is(x, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- rowRanges(x)
        } else {
            args[["rowData"]] <- rowData(x)
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }



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
        x,
        fun  # nolint
    ) {
        validObject(x)
        assert(isString(fun))
        ## Remap cellular barcodes.
        colData <- colData(x)
        assert(
            isSubset(c("sampleID", "aggregate"), colnames(colData)),
            is.factor(colData[["aggregate"]])
        )
        cli_alert(sprintf(
            "Remapping cells to aggregate samples: %s",
            toString(sort(levels(colData[["aggregate"]])), width = 100L)
        ))
        map <- colData(x)
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
        assert(identical(names(by), colnames(x)))
        colData(x)[["aggregate"]] <- by

        ## Generate SingleCellExperiment ---------------------------------------
        ## Using `SummarizedExperiment` method here.
        rse <- as(x, "RangedSummarizedExperiment")
        colData(rse)[["sampleID"]] <- NULL
        rse <- aggregateCols(x = rse, fun = fun)
        assert(
            is(rse, "RangedSummarizedExperiment"),
            identical(nrow(rse), nrow(x))
        )
        ## Update the sample data.
        colData <- colData(rse)
        assert(isSubset(rownames(colData), names(cell2sample)))
        colData[["sampleID"]] <- cell2sample[rownames(colData)]
        colData[["sampleName"]] <- colData[["sampleID"]]
        colData(rse) <- colData
        ## Update the metadata.
        metadata <- metadata(x)
        metadata[["aggregate"]] <- TRUE
        metadata[["aggregateCols"]] <- by
        ## Now ready to generate aggregated SCE.
        sce <- SingleCellExperiment(
            assays = SimpleList(counts = counts(rse)),
            rowRanges = rowRanges(x),
            colData = colData(rse),
            metadata = list(
                aggregate = TRUE,
                aggregateCols = by,
                interestingGroups = interestingGroups(x)
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
