#' @name sampleData
#' @inherit bioverbs::sampleData
#' @note Updated 2019-08-16.
#'
#' @section All supported S4 classes:
#'
#' Illegal `colData`:
#'
#' - `interestingGroups`: Generated automatically, based on the criteria
#'   slotted into the object using [interestingGroups()]. The function will
#'   error intentionally if this column is manually defined in [colData()].
#'
#' Recommended `colData`:
#'
#' - `sampleName`: Human readable sample names used by basejump plotting
#'   functions in favor of object column names, which should be syntactically
#'   valid (but not always very readable). See
#'   [`make.names()`][base::make.names] for more information on syntactically
#'   valid names. Note that if this column is not defined in the object,
#'   it will be returned automatically by [sampleData()].
#'
#' @section SummarizedExperiment:
#'
#' Required `colData`:
#'
#' - None.
#'
#' Illegal `colData`:
#'
#' - `sampleID`: Redundant; already defined in the object column names.
#'
#' @section SingleCellExperiment:
#'
#' Recommended `colData`:
#'
#' - `sampleID`: `factor` defining cell-to-sample mappings. These mappings
#'   should use syntactically valid names. Note that this is not currently
#'   required as we're supporting `SingleCellExperiment` objects from 1 sample,
#'   but it's required for working with multiple samples in a single object.
#'
#' @inheritParams acidroxygen::params
#' @param clean `logical(1)`.
#'   Only return `factor` columns. Useful when working with objects that contain
#'   quality control metrics in [`colData()`][SummarizedExperiment::colData].
#'   For example, `bcbioRNASeq` and `DESeqDataSet` objects often contain
#'   additional columns that aren't informative sample metadata.
#' @param ignoreCols `character` or `NULL`.
#'   Only applies when `clean = TRUE`. Additional factor columns defined in
#'   `colData` to be ignored as sample-level metadata. Particularly useful for
#'   `SingleCellExperiment` objects, where cell-to-sample mappings are defined
#'   using the `sampleID` column.
#' @param blacklistCols `character` or `NULL`.
#'   Column names that should not be treated as sample-level metadata.
#'   Currently applicable only to `SingleCellExperiment` objects, which have
#'   cell-level columns that can be difficult to distinguish, especially when
#'   processed using Seurat, scater, etc.
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## SummarizedExperiment ====
#' x <- rse
#' sampleData(x)
#'
#' ## Assignment support
#' sampleData(x)[["batch"]] <- 1L
#' ## `batch` column should be now defined.
#' sampleData(x)
#'
#' ## SingleCellExperiment ====
#' x <- sce
#' sampleData(x)
#'
#' ## Assignment support.
#' sampleData(x)[["batch"]] <- 1L
#' ## `batch` column should be now defined.
#' sampleData(x)
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom bioverbs sampleData
#' @usage sampleData(object, ...)
#' @export
NULL

#' @rdname sampleData
#' @name sampleData<-
#' @importFrom bioverbs sampleData<-
#' @usage sampleData(object, ...) <- value
#' @export
NULL



## Don't run validity checks here.
## Updated 2019-08-16.
`sampleData,SummarizedExperiment` <-  # nolint
    function(
        object,
        clean = TRUE,
        ignoreCols = c(
            "^description$",
            "^genomeBuild$",
            "^qualityFormat$",
            "^samRef$"
        )
    ) {
        data <- colData(object)
        if (!hasRows(data)) return(data)
        assert(
            hasRownames(data),
            isFlag(clean),
            isCharacter(ignoreCols, nullOK = TRUE),
            areDisjointSets(x = colnames(data), y = metadataBlacklist)
        )
        ## Require `sampleName` column.
        if (!isSubset("sampleName", colnames(data))) {
            data[["sampleName"]] <- as.factor(rownames(data))
        } else if (!is.factor(data[["sampleName"]])) {
            stop("'sampleData()' requires 'sampleName' factor in 'colData()'.")
        }
        ## Clean mode.
        if (isTRUE(clean)) {
            ## Return only a subset of factor columns.
            keep <- bapply(X = data, FUN = is.factor)
            data <- data[, keep, drop = FALSE]
            ## Drop any additional uninformative columns to ignore.
            if (is.character(ignoreCols)) {
                keep <- !grepl(
                    pattern = paste(ignoreCols, collapse = "|"),
                    x = camelCase(colnames(data))
                )
                data <- data[, keep, drop = FALSE]
            }
        }
        ## Add interesting groups column.
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )
        ## Return.
        assert(
            is.factor(data[["interestingGroups"]]),
            is.factor(data[["sampleName"]])
        )
        data
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SummarizedExperiment"),
    definition = `sampleData,SummarizedExperiment`
)



## Don't run validity checks here.
## Updated 2019-08-16.
`sampleData,SingleCellExperiment` <-  # nolint
    function(
        object,
        clean = TRUE,
        ignoreCols = c(
            "^description$",
            "^genomeBuild$",
            "^qualityFormat$",
            "^samRef$"
        ),
        blacklistCols = c(
            "^ident$",
            "^g2mScore$",
            "^sScore$",
            "^phase$",
            "^oldIdent$",
            "^origIdent$",
            "^res[0-9]+"
        )
    ) {
        data <- colData(object)
        if (!hasRows(data)) return(data)
        assert(
            hasRownames(data),
            isFlag(clean),
            isCharacter(ignoreCols, nullOK = TRUE),
            isCharacter(blacklistCols, nullOK = TRUE),
            areDisjointSets("interestingGroups", colnames(data))
        )
        interestingGroups <- matchInterestingGroups(object)

        ## Prepare columns -----------------------------------------------------
        ## Generate `sampleID` and `sampleName` columns, if necessary. We're not
        ## requiring `sampleID` because many SingleCellExperiment objects are
        ## derived from a single sample (e.g. 10X PBMC example data). Note that
        ## `SummarizedExperiment` method differs by not allowing the `sampleID`
        ## column, which are the `colnames` of the object.
        ## `SingleCellExperiment` maps cells to `colnames` instead of samples,
        ## so this factor is necessary when handling multiple samples.
        if (!"sampleID" %in% colnames(data)) {
            data[["sampleID"]] <- factor("unknown")
        }
        assert(is.factor(data[["sampleID"]]))
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- data[["sampleID"]]
        }
        assert(is.factor(data[["sampleName"]]))
        ## Drop any blacklisted cell-level columns.
        if (is.character(blacklistCols)) {
            keep <- !grepl(
                pattern = paste(blacklistCols, collapse = "|"),
                x = camelCase(colnames(data))
            )
            data <- data[, keep, drop = FALSE]
        }

        ## Clean mode ----------------------------------------------------------
        if (isTRUE(clean)) {
            ## Return only a subset of factor columns.
            keep <- bapply(X = data, FUN = is.factor)
            data <- data[, keep, drop = FALSE]
            ## Drop any additional uninformative columns to ignore.
            if (is.character(ignoreCols)) {
                keep <- !grepl(
                    pattern = paste(ignoreCols, collapse = "|"),
                    x = camelCase(colnames(data))
                )
                data <- data[, keep, drop = FALSE]
            }
        }

        ## Drop rows with too many uniques (cell level) ------------------------
        nSamples <- length(unique(data[["sampleID"]]))
        assert(all(isPositive(nSamples)))

        ## Keep columns that have have less than or equal the same number of
        ## uniques as the the number of samples. Note that this step is really
        ## essential, especially when QC metrics are slotted into `colData()`.
        keep <- bapply(
            X = data,
            FUN = function(x) {
                length(unique(x)) <= nSamples
            }
        )
        data <- data[, keep, drop = FALSE]

        ## Check rows with same number of uniques ------------------------------
        ## For columns that have the exact same number of uniques as the number
        ## of samples, they need to match our `sampleID` column factor levels
        ## exactly, otherwise we can run into issues where cell-level values
        ## appear to be sample level. Create a factor integer table to check for
        ## this. Shouldn't apply too often but can happen for some edge cases.
        ftable <- data
        keep <- bapply(
            X = ftable,
            FUN = function(x) {
                length(unique(x)) == nSamples
            }
        )
        ftable <- ftable[, keep, drop = FALSE]
        ## Don't use `as.factor()` to `as.integer()` chain here. If you have
        ## factors with different levels when sorted alphabetically, this will
        ## not work as expected.
        ##
        ## Example edge case:
        ## - sampleID: sample2, sample
        ## - sampleName: a, b
        ##
        ## See how these factor levels will flip in the index and not match.
        ## Instead, we need to enforce the factor levels by order of appearance.
        ##
        ## ftable here represents a numeric factor index table.
        factorToInteger <- function(x) {
            as.integer(factor(x, levels = unique(x)))
        }
        ftable <- mutate_all(ftable, factorToInteger)
        trash <- !bapply(
            X = ftable,
            FUN = function(x) {
                identical(x, ftable[["sampleID"]])
            }
        )
        if (any(trash)) {
            keep <- setdiff(colnames(data), names(trash[trash]))
            data <- data[, keep, drop = FALSE]
        }
        assert(isSubset(c("sampleID", interestingGroups), colnames(data)))

        ## Collapse to sample level --------------------------------------------
        ## Collapse and set the row names to `sampleID`.
        rownames(data) <- NULL
        data <- unique(data)
        if (
            nrow(data) > nSamples ||
            any(duplicated(data[["sampleID"]]))
        ) {
            stop(sprintf(
                fmt = paste(
                    "Failed to collapse 'colData()' to sample level.\n",
                    "Check: %s.",
                    sep = "\n"
                ),
                toString(colnames(data), width = 200L)
            ))
        }
        rownames(data) <- data[["sampleID"]]
        ## Returning arranged by `sampleID`. Use `setdiff()` approach instead of
        ## `NULL` assignment on `sampleID` column to maintain backwards
        ## compatibility prior to BioC 3.8.
        data <- data[
            rownames(data),
            setdiff(colnames(data), "sampleID"),
            drop = FALSE
        ]
        ## Add interesting groups column.
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = interestingGroups
        )
        ## Return.
        assert(
            is.factor(data[["interestingGroups"]]),
            is.factor(data[["sampleName"]])
        )
        data
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SingleCellExperiment"),
    definition = `sampleData,SingleCellExperiment`
)



## nolint start
##
## Note that attempting to use `NULL` to remove columns on a DataFrame
## will result in `S4Vectors::V_recycle()` errors, prior to BioC 3.8.
## https://stat.ethz.ch/pipermail/bioc-devel/2017-November/012343.html
##
## nolint end



## Updated 2019-08-08.
`sampleData<-,SummarizedExperiment,DataFrame` <-  # nolint
    function(object, value) {
        assert(hasRownames(value))
        blacklist <- c("interestingGroups", "rowname", "sampleID")
        keep <- setdiff(colnames(value), blacklist)
        assert(hasLength(keep))
        value <- value[, keep, drop = FALSE]
        colData(object) <- value
        validObject(object)
        object
    }



#' @rdname sampleData
#' @export
setReplaceMethod(
    f = "sampleData",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = `sampleData<-,SummarizedExperiment,DataFrame`
)



## Updated 2019-07-22.
`sampleData<-,SingleCellExperiment,DataFrame` <-  # nolint
    function(object, value) {
        assert(hasRownames(value))
        blacklist <- c("interestingGroups", "rowname", "sampleID")
        keep <- setdiff(colnames(value), blacklist)
        assert(hasLength(keep))
        value <- value[, keep, drop = FALSE]
        value[["sampleID"]] <- as.factor(rownames(value))
        colData <- colData(object)
        assert(isSubset("sampleID", colnames(colData)))
        colData <- colData[
            ,
            c("sampleID", setdiff(colnames(colData), colnames(value))),
            drop = FALSE
        ]
        value <- left_join(colData, value, by = "sampleID")
        assert(is(value, "DataFrame"), hasRownames(value))
        colData(object) <- value
        object
    }



#' @rdname sampleData
#' @export
setReplaceMethod(
    f = "sampleData",
    signature = signature(
        object = "SingleCellExperiment",
        value = "DataFrame"
    ),
    definition = `sampleData<-,SingleCellExperiment,DataFrame`
)
