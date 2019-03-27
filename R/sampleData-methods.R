# NB: Don't use `NULL` assignment method to remove columns on a DataFrame if
# we're providing support for BioC releases prior to 3.8. This will result in a
# metadata problem that returns as a cryptic `S4Vectors::V_recycle` error.
# This will pop up: 'NROW(value)' is greater than 'length(x)'.



#' @name sampleData
#' @inherit bioverbs::sampleData
#' @inheritParams params
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
#'
#' @examples
#' data(rse, sce, package = "acidData")
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



#' @importFrom bioverbs sampleData
#' @aliases NULL
#' @export
bioverbs::sampleData

#' @importFrom bioverbs sampleData<-
#' @aliases NULL
#' @export
bioverbs::`sampleData<-`



# Don't run validity checks here.
# Note that we're using grep pattern matching for `ignoreCols`.
sampleData.SummarizedExperiment <-  # nolint
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
        assert(
            hasRownames(data),
            isFlag(clean),
            isCharacter(ignoreCols, nullOK = TRUE)
        )

        # Prepare columns ------------------------------------------------------
        assert(areDisjointSets(colnames(data), metadataBlacklist))

        # Require `sampleName` column.
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- as.factor(rownames(data))
        }
        assert(is.factor(data[["sampleName"]]))

        # Clean mode -----------------------------------------------------------
        if (isTRUE(clean)) {
            # Return only a subset of factor columns.
            keep <- bapply(X = data, FUN = is.factor)
            data <- data[, keep, drop = FALSE]

            # Drop any additional uninformative columns to ignore.
            if (is.character(ignoreCols)) {
                keep <- !grepl(
                    pattern = paste(ignoreCols, collapse = "|"),
                    x = colnames(data)
                )
                data <- data[, keep, drop = FALSE]
            }
        }

        # Interesting groups ---------------------------------------------------
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )

        # Return ---------------------------------------------------------------
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
    definition = sampleData.SummarizedExperiment
)



# Don't run validity checks here.
sampleData.SingleCellExperiment <-  # nolint
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
            "^G2M.Score$",
            "^Phase$",
            "^S.Score$",
            "^ident$",
            "^old.ident$",
            "^orig.ident$",
            "^res[.0-9]+$"
        )
    ) {
        data <- colData(object)
        assert(
            hasRownames(data),
            isFlag(clean),
            isCharacter(ignoreCols, nullOK = TRUE),
            isCharacter(blacklistCols, nullOK = TRUE)
        )

        # Prepare columns ------------------------------------------------------
        assert(areDisjointSets("interestingGroups", colnames(data)))

        # Generate `sampleID` and `sampleName` columns, if necessary. We're not
        # requiring `sampleID` because many SingleCellExperiment objects are
        # derived from a single sample (e.g. 10X PBMC example data). Note that
        # `SummarizedExperiment` method differs by not allowing the `sampleID`
        # column, which are the `colnames` of the object. `SingleCellExperiment`
        # maps cells to `colnames` instead of samples, so this factor is
        # necessary when handling multiple samples.
        if (!"sampleID" %in% colnames(data)) {
            data[["sampleID"]] <- factor("unknown")
        }
        assert(is.factor(data[["sampleID"]]))
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- data[["sampleID"]]
        }
        assert(is.factor(data[["sampleName"]]))

        # Blacklist ------------------------------------------------------------
        # Drop any blacklisted cell-level columns.
        if (is.character(blacklistCols)) {
            keep <- !grepl(
                pattern = paste(blacklistCols, collapse = "|"),
                x = colnames(data)
            )
            data <- data[, keep, drop = FALSE]
        }

        # Clean mode -----------------------------------------------------------
        if (isTRUE(clean)) {
            # Return only a subset of factor columns.
            keep <- bapply(X = data, FUN = is.factor)
            data <- data[, keep, drop = FALSE]

            # Drop any additional uninformative columns to ignore.
            if (is.character(ignoreCols)) {
                keep <- !grepl(
                    pattern = paste(ignoreCols, collapse = "|"),
                    x = colnames(data)
                )
                data <- data[, keep, drop = FALSE]
            }
        }

        # Drop rows with too many uniques (cell level) -------------------------
        nSamples <- length(unique(data[["sampleID"]]))
        assert(all(isPositive(nSamples)))

        # Keep columns that have have less than or equal the same number of
        # uniques as the the number of samples. Note that this step is really
        # essential, especially when QC metrics are slotted into `colData()`.
        keep <- bapply(
            X = data,
            FUN = function(x) {
                length(unique(x)) <= nSamples
            }
        )
        data <- data[, keep, drop = FALSE]

        # Check rows with same number of uniques -------------------------------
        # For columns that have the exact same number of uniques as the number
        # of samples, they need to match our `sampleID` column factor levels
        # exactly, otherwise we can run into issues where cell-level values
        # appear to be sample level. Create a factor integer table to check for
        # this. Shouldn't apply too often but can happen for some edge cases.
        levelTbl <- data
        keep <- bapply(
            X = levelTbl,
            FUN = function(x) {
                length(unique(x)) == nSamples
            }
        )
        levelTbl %<>%
            .[, keep, drop = FALSE] %>%
            as_tibble(rownames = NULL) %>%
            mutate_all(as.factor) %>%
            mutate_all(as.integer)
        trash <- !bapply(
            X = levelTbl,
            FUN = function(x) {
                identical(x, levelTbl[["sampleID"]])
            }
        )
        if (any(trash)) {
            keep <- setdiff(colnames(data), names(trash[trash]))
            data <- data[, keep, drop = FALSE]
        }

        # Collapse to sample level ---------------------------------------------
        # Collapse and set the row names to `sampleID`.
        rownames(data) <- NULL
        data <- unique(data)
        if (
            nrow(data) > nSamples ||
            any(duplicated(data[["sampleID"]]))
        ) {
            stop(paste0(
                "Failed to collapse `colData` to sample level.\n",
                "Check these columns: ",
                toString(colnames(data), width = 200L)
            ))
        }
        rownames(data) <- data[["sampleID"]]

        # Returning arranged by `sampleID`.
        # Use `setdiff()` approach instead of `NULL` assignment on `sampleID`
        # column to maintain backwards compatibility prior to BioC 3.8.
        data <- data[
            rownames(data),
            setdiff(colnames(data), "sampleID"),
            drop = FALSE
        ]

        # Interesting groups ---------------------------------------------------
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )

        # Return ---------------------------------------------------------------
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
    definition = sampleData.SingleCellExperiment
)



`sampleData<-.SummarizedExperiment` <-  # nolint
    function(object, value) {
        # Don't allow blacklisted columns.
        # Note that attempting to use `NULL` to remove columns on a DataFrame
        # will result in `S4Vectors::V_recycle()` errors, prior to BioC 3.8.
        # https://stat.ethz.ch/pipermail/bioc-devel/2017-November/012343.html
        blacklist <- c(
            "interestingGroups",
            "rowname",
            "sampleID"
        )
        keep <- setdiff(colnames(value), blacklist)
        assert(hasLength(keep))
        # Be sure to include `drop` here in case we have only 1 column left.
        value <- value[, keep, drop = FALSE]
        # Now safe to assign and return.
        colData(object) <- value
        validObject(object)
        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = `sampleData<-.SummarizedExperiment`
)



`sampleData<-.SingleCellExperiment` <-  # nolint
    function(object, value) {
        assert(is(value, "DataFrame"))

        # Remove legacy `sampleData` in metadata, if defined.
        if (!is.null(metadata(object)[["sampleData"]])) {
            message("Removed legacy sampleData in metadata() slot.")
            metadata(object)[["sampleData"]] <- NULL
        }

        # Don't allow blacklisted columns.
        value[["interestingGroups"]] <- NULL
        value[["rowname"]] <- NULL
        value[["sampleID"]] <- NULL

        # Generate `sampleID` column.
        assert(hasRownames(value))
        value[["sampleID"]] <- as.factor(rownames(value))

        # Update colData slot.
        colData <- colData(object)
        assert(isSubset("sampleID", colnames(colData)))
        colData <- colData[
            ,
            c("sampleID", setdiff(colnames(colData), colnames(value))),
            drop = FALSE
        ]

        # Join the sample-level metadata into cell-level colData.
        # Use BiocTibble left_join DataFrame method here.
        join <- left_join(
            x = as_tibble(colData, rownames = "rowname"),
            y = as_tibble(value, rownames = NULL),
            by = "sampleID"
        )
        value <- as(join, "DataFrame")
        assert(hasRownames(value))
        colData(object) <- value

        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SingleCellExperiment",
        value = "DataFrame"
    ),
    definition = `sampleData<-.SingleCellExperiment`
)
