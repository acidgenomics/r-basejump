#' @name aggregateCols
#' @inherit AcidGenerics::aggregateCols
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2021-01-16.
#'
#' @inherit aggregateRows
#' @param ... Additional arguments.
#'
#' @examples
#' counts <- matrix(
#'     data = c(
#'         0L, 2L, 2L, 2L,
#'         2L, 0L, 2L, 2L,
#'         2L, 2L, 0L, 2L,
#'         2L, 2L, 2L, 0L
#'     ),
#'     nrow = 4L,
#'     ncol = 4L,
#'     byrow = TRUE,
#'     dimnames = list(
#'         paste0("transcript", seq_len(4L)),
#'         paste(
#'             paste0("sample", rep(seq_len(2L), each = 2L)),
#'             paste0("replicate", rep(seq_len(2L), times = 2L)),
#'             sep = "_"
#'         )
#'     )
#' )
#'
#' samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
#' names(samples) <- colnames(counts)
#' print(samples)
#'
#' ## matrix ====
#' print(counts)
#' aggregateCols(counts, by = samples)
#'
#' ## Matrix ====
#' sparse <- as(counts, "sparseMatrix")
#' print(sparse)
#' aggregateCols(sparse, by = samples)
#'
#' ## SummarizedExperiment ====
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assays = SimpleList(counts = counts),
#'     colData = DataFrame(
#'         sampleName = as.factor(names(samples)),
#'         aggregate = samples
#'     )
#' )
#' print(se)
#' aggregateCols(se)
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



#' @rdname aggregateCols
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



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("Matrix"),
    definition = `aggregateCols,Matrix`
)



## Updated 2020-05-22.
`aggregateCols,SummarizedExperiment` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
        validObject(x)
        assert(
            hasDimnames(x),
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



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateCols,SummarizedExperiment`
)



## Updated 2021-01-16.
`aggregateCols,SingleCellExperiment` <-  # nolint
    function(
        x,
        fun  # nolint
    ) {
        validObject(x)
        assert(isString(fun))
        ## Remap cellular barcodes.
        colData <- colData(x)
        sampleCol <- matchSampleColumn(colData)
        cellCol <- "cellId"
        aggregateCol <- "aggregate"
        assert(
            isString(sampleCol),
            isString(aggregateCol),
            isSubset(c(sampleCol, aggregateCol), colnames(colData)),
            is.factor(colData[[aggregateCol]])
        )
        cli_alert(sprintf(
            "Remapping cells to aggregate samples: %s",
            toString(sort(levels(colData[[aggregateCol]])), width = 100L)
        ))
        map <- as_tibble(colData(x), rownames = cellCol)
        ## Check to see if we can aggregate.
        if (!all(mapply(
            FUN = grepl,
            x = map[[cellCol]],
            pattern = paste0("^", map[[sampleCol]]),
            SIMPLIFY = TRUE
        ))) {
            stop("Cell identifiers are not prefixed with sample identifiers.")
        }
        by <- mapply(
            FUN = gsub,
            x = map[[cellCol]],
            pattern = paste0("^", map[[sampleCol]]),
            replacement = map[[aggregateCol]],
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
        by <- as.factor(by)
        cell2sample <- as.factor(map[[aggregateCol]])
        names(cell2sample) <- as.character(by)
        ## Reslot the `aggregate` column using these groupings.
        assert(identical(names(by), colnames(x)))
        colData(x)[[aggregateCol]] <- by
        ## Generate SingleCellExperiment ---------------------------------------
        ## Using `SummarizedExperiment` method here.
        rse <- as(x, "RangedSummarizedExperiment")
        colData(rse)[[sampleCol]] <- NULL
        rse <- aggregateCols(x = rse, fun = fun)
        assert(
            is(rse, "RangedSummarizedExperiment"),
            identical(nrow(rse), nrow(x))
        )
        ## Update the sample data.
        colData <- colData(rse)
        assert(isSubset(rownames(colData), names(cell2sample)))
        colData[[sampleCol]] <- cell2sample[rownames(colData)]
        if (isSubset("sampleName", colnames(colData))) {
            colData[["sampleName"]] <- colData[[sampleCol]]
        }
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



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCols,SingleCellExperiment`
)
