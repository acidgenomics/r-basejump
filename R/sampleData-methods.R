#' Sample Data
#'
#' Metadata that describes the samples.
#'
#' This is a complement to the standard [colData()] function, but improves
#' support for accessing sample metadata for datasets where multiple items in
#' the columns map to a single sample (e.g. cells for a single-cell RNA-seq
#' experiment).
#'
#' @name sampleData
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- rse_small
#' sampleData(x)
#'
#' # Assignment support
#' sampleData(x)[["batch"]] <- 1L
#' # `batch` column should be now defined.
#' sampleData(x)
#'
#' # SingleCellExperiment ====
#' x <- sce_small
#' sampleData(x)
#'
#' # Assignment support.
#' sampleData(x)[["batch"]] <- 1L
#' # `batch` column should be now defined.
#' sampleData(x)
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

        # Generate `interestingGroups` column.
        data[["interestingGroups"]] <- NULL
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )

        data
    }



`.sampleData<-.SE` <-  # nolint
    function(object, value) {
        # Don't allow blacklisted columns.
        value[["interestingGroups"]] <- NULL
        value[["rowname"]] <- NULL
        value[["sampleID"]] <- NULL
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
        if (!"sampleID" %in% colnames(data)) {
            stop(paste(
                "`sampleID` column must be defined in `colData()` slot"
            ), call. = FALSE)
        }
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- data[["sampleID"]]
        }

        # Sample-level columns -------------------------------------------------
        nSamples <- length(unique(data[["sampleID"]]))
        assert_all_are_positive(nSamples)

        # `clusterCols` defines clustering-specific columns that should be
        # dropped. These include columns that map cells to cell types, etc.
        keep <- !grepl(
            pattern = paste(clusterCols, collapse = "|"),
            x = camel(colnames(data))
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
        factortbl <- subset %>%
            as_tibble(rownames = NULL) %>%
            mutate_all(as.factor) %>%
            mutate_all(as.integer)
        trash <- !vapply(
            X = factortbl,
            FUN = function(x) {
                identical(x, factortbl$sampleID)
            },
            FUN.VALUE = logical(1L)
        )
        trash <- names(trash[trash])
        data <- data[, setdiff(colnames(data), trash), drop = FALSE]

        # Collapse and set the rownames to `sampleID`.
        rownames(data) <- NULL
        data <- unique(data)
        assert_has_no_duplicates(data[["sampleID"]])
        rownames(data) <- data[["sampleID"]]
        data[["sampleID"]] <- NULL
        # Return sorted by `sampleID`.
        data <- data[rownames(data), , drop = FALSE]

        # Generate `interestingGroups` column.
        data[["interestingGroups"]] <- NULL
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )

        data
    }



`.sampleData<-.SCE` <-  # nolint
    function(object, value) {
        # Remove legacy `sampleData` in metadata, if defined.
        if (!is.null(metadata(object)[["sampleData"]])) {
            message("Removing legacy `sampleData` in `metadata()` slot")
            metadata(object)[["sampleData"]] <- NULL
        }

        # Don't allow blacklisted columns.
        value[["interestingGroups"]] <- NULL
        value[["rowname"]] <- NULL
        value[["sampleID"]] <- NULL

        # Generate `sampleID` column.
        value[["sampleID"]] <- as.factor(rownames(value))

        # Update colData slot.
        colData <- colData(object)
        assert_is_subset("sampleID", colnames(colData))
        colData <- colData[
            ,
            c("sampleID", setdiff(colnames(colData), colnames(value))),
            drop = FALSE
        ]
        colData <- left_join(
            x = colData,
            y = value,
            by = "sampleID"
        )
        colData(object) <- colData

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
