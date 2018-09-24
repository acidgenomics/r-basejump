#' Sample Data
#'
#' Return the sample metadata. Columns are always sanitized to factor.
#'
#' @note This is a complement to the standard [colData()] function, but improves
#'   support for accessing sample metadata for datasets where multiple items in
#'   the columns map to a single sample (e.g. cells for a single-cell RNA-seq
#'   experiment).
#'
#' @section SingleCellExperiment:
#'
#' Metadata in columns describing the samples, which are defined in the
#' rownames. Similar to [colData()], which for `bcbioSingleCell` and
#' `SingleCellExperiment` objects describes cells in the columns, rather than
#' the samples.
#'
#' @name sampleData
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `DataFrame`. Metadata that describes the samples.
#'
#' @examples
#' # SummarizedExperiment ====
#' sampleData(rse_small)
#'
#' # Assignment support
#' x <- rse_small
#' sampleData(x)[["test"]] <- seq_len(ncol(x))
#' # `test` column should be now defined
#' sampleData(x)
#'
#' # SingleCellExperiment ====
#' x <- sce_small
#' sampleData(x) %>% glimpse()
#' sampleData(x)[["batch"]] <- 1L
#' sampleData(x) %>% glimpse()
#' "batch" %in% colnames(colData(x))
NULL



.sampleData.SE <-  # nolint
    function(object) {
        validObject(object)
        data <- colData(object)
        assertHasRownames(data)
        # Require `sampleName` column to be defined.
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- as.factor(rownames(data))
        }
        # Generate `interestingGroups` column, if necessary.
        if (!"interestingGroups" %in% colnames(data)) {
            data <- uniteInterestingGroups(
                object = data,
                interestingGroups = matchInterestingGroups(object)
            )
        }
        data
    }



`.sampleData<-.SE` <-  # nolint
    function(object, value) {
        # Check for blacklisted columns.
        assert_are_disjoint_sets(
            colnames(value),
            c("rowname", "sampleID")
        )

        # Reslot the interesting groups column automatically.
        value[["interestingGroups"]] <- NULL
        value <- uniteInterestingGroups(
            object = value,
            interestingGroups = matchInterestingGroups(object)
        )

        # Now safe to assign and return.
        colData(object) <- value
        validObject(object)
        object
    }



.sampleData.SCE <-  # nolint
    function(object) {
        data <- colData(object)

        # Require `sampleID` and `sampleName` columns.
        # Note that `SummarizedExperiment` method differs but not requiring
        # the `sampleID` column, which are the `colnames` of the object.
        # `SingleCellExperiment` maps cells to `colnames` instead of samples.
        if (!all(c("sampleID", "sampleName") %in% colnames(data))) {
            stop(paste(
                "`sampleData()` requires `sampleID` and `sampleName` columns",
                "to be defined in `colData()`"
            ), call. = FALSE)
        }

        # Get the number of samples.
        nSamples <- length(unique(data[["sampleID"]]))
        assert_all_are_positive(nSamples)

        # Select only columns that map to samples.
        isSampleLevel <- vapply(
            X = data,
            FUN = function(x) {
                uniques <- length(unique(x))
                uniques <= nSamples
            },
            FUN.VALUE = logical(1L)
        )
        data <- data[, isSampleLevel, drop = FALSE]

        # `clusterCols` used to define cluster mappings are blacklisted.
        data <- data[
            ,
            !grepl(
                pattern = paste(clusterCols, collapse = "|"),
                x = camel(colnames(data))
            ),
            drop = FALSE
            ]

        # Collapse and set the rownames to `sampleID`.
        rownames(data) <- NULL
        data <- unique(data)
        assert_has_no_duplicates(data[["sampleID"]])
        rownames(data) <- data[["sampleID"]]
        data[["sampleID"]] <- NULL
        # Return sorted by `sampleID`.
        data <- data[rownames(data), , drop = FALSE]

        if (!"interestingGroups" %in% colnames(data)) {
            interestingGroups <- interestingGroups(object)
            if (length(interestingGroups) > 0L) {
                data <- uniteInterestingGroups(data, interestingGroups)
            }
        }

        data
    }



`.sampleData<-.SCE` <-  # nolint
    function(object, value) {
        # Don't allow the user to manually set sampleID column.
        value[["sampleID"]] <- rownames(value)

        # Ensure the interesting groups column is not stashed.
        value[["interestingGroups"]] <- NULL
        interestingGroups <- interestingGroups(object)
        if (length(interestingGroups) > 0L) {
            value <- uniteInterestingGroups(value, interestingGroups)
        }

        # Ensure the cell-level column data is also updated.
        colData <- colData(object)
        # Require that sampleID column, defined by `cell2sample()`, is present.
        assert_is_subset("sampleID", colnames(colData))
        colData <- colData[
            ,
            c("sampleID", setdiff(colnames(colData), colnames(value))),
            drop = FALSE
            ]
        colData[["cellID"]] <- rownames(colData)
        colData <- merge(
            x = colData,
            y = value,
            by = "sampleID",
            all.x = TRUE
        )
        rownames(colData) <- colData[["cellID"]]
        colData[["cellID"]] <- NULL

        # Re-slot the cell-level data.
        colData <- colData[colnames(object), , drop = FALSE]
        colData(object) <- colData

        # Remove legacy `sampleData` in metadata, if defined.
        if (!is.null(metadata(object)[["sampleData"]])) {
            message("Removing legacy `sampleData` in `metadata()` slot")
            metadata(object)[["sampleData"]] <- NULL
        }

        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SummarizedExperiment"),
    definition = .sampleData.SE
)



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = `.sampleData<-.SE`
)




#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SingleCellExperiment"),
    definition = .sampleData.SCE
)



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SingleCellExperiment",
        value = "DataFrame"
    ),
    definition = `.sampleData<-.SCE`
)
