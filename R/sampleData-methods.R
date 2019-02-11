# NB: Don't use `NULL` assignment method to remove columns on a DataFrame if
# we're providing support for BioC releases prior to 3.8. This will result in a
# metadata problem that returns as a cryptic `S4Vectors::V_recycle` error.
# This will pop up: 'NROW(value)' is greater than 'length(x)'.



#' @name sampleData
#' @inherit bioverbs::sampleData
#' @inheritParams params
#'
#' @param clean `logical(1)`.
#'   Only return `factor` columns. Useful when working with objects that contain
#'   metrics in [`colData()`][SummarizedExperiment::colData].
#' @param blacklist `character`.
#'   Column names that should never be treated as sample-level metadata.
#'   Applicable only to `SingleCellExperiment` objects.
#'
#' @examples
#' data(rse, sce)
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



# Checking for illegal user metadata in colData.
# Don't blacklist `sampleID` here because it's allowed for SCE method.
.stopOnSampleDataBlacklist <- function(object) {
    assert(
        is(object, "DataFrame"),
        hasRownames(object)
    )
    # Check for user stash of `interestingGroups` column.
    if (isSubset("interestingGroups", colnames(object))) {
        stop(paste(
            "The `interestingGroups` column should not be manually defined",
            "in `colData()`. This column gets generated automatically."
        ))
    }
    TRUE
}



# Don't run validity checks here.
sampleData.SummarizedExperiment <-  # nolint
    function(object, clean = FALSE) {
        data <- colData(object)
        .stopOnSampleDataBlacklist(data)

        # Require `sampleName` column.
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- as.factor(rownames(data))
        }

        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )
        # Return only factor columns in clean mode.
        if (isTRUE(clean)) {
            keep <- vapply(
                X = data,
                FUN = is.factor,
                FUN.VALUE = logical(1L)
            )
            data <- data[, keep, drop = FALSE]
        }

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
        blacklist = c(
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
        .stopOnSampleDataBlacklist(data)
        assert(isCharacter(blacklist))

        # Require `sampleID` and `sampleName` columns.
        # Note that `SummarizedExperiment` method differs but not requiring
        # the `sampleID` column, which are the `colnames` of the object.
        # `SingleCellExperiment` maps cells to `colnames` instead of samples.
        if (!"sampleID" %in% colnames(data)) {
            data[["sampleID"]] <- factor("unknown")
        }
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- data[["sampleID"]]
        }

        # Sample-level columns -------------------------------------------------
        nSamples <- length(unique(data[["sampleID"]]))
        assert(all(isPositive(nSamples)))

        # Drop any blacklisted columns.
        keep <- !grepl(
            pattern = paste(blacklist, collapse = "|"),
            x = colnames(data)
        )
        data <- data[, keep, drop = FALSE]

        # Keep columns that have fewer uniques than the number of samples.
        keep <- vapply(
            X = data,
            FUN = function(x) {
                length(unique(x)) <= nSamples
            },
            FUN.VALUE = logical(1L)
        )
        data <- data[, keep, drop = FALSE]

        # For columns that have the same number of uniques, they need to match
        # our `sampleID` column factor levels exactly. Create a factor integer
        # table to check for this.
        subset <- data[
            ,
            vapply(
                X = data,
                FUN = function(x) {
                    length(unique(x)) == nSamples
                },
                FUN.VALUE = logical(1L)
            ),
            drop = FALSE
            ]
        # Make the factor integer table.
        factortbl <- subset %>%
            as_tibble(rownames = NULL) %>%
            mutate_all(as.factor) %>%
            mutate_all(as.integer)
        trash <- !vapply(
            X = factortbl,
            FUN = function(x) {
                identical(x, factortbl[["sampleID"]])
            },
            FUN.VALUE = logical(1L)
        )
        keep <- setdiff(colnames(data), names(trash[trash]))
        data <- data[, keep, drop = FALSE]

        # Collapse and set the rownames to `sampleID`.
        rownames(data) <- NULL
        data <- unique(data)
        if (
            nrow(data) > nSamples ||
            any(duplicated(data[["sampleID"]]))
        ) {
            stop(paste(
                "Failed to collapse `colData` to sample level.",
                "Check these columns:", toString(colnames(data))
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

        # Ensure `interestingGroups` column is generated.
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
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
