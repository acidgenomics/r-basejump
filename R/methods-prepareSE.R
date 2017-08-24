#' Prepare SummarizedExperiment
#'
#' This is a utility wrapper for `SummarizedExperiment()` that provides
#' automatic subsetting for `colData` and `rowData`.
#'
#' `prepareSE()` also provides automatic metadata slotting of multiple useful
#' environment parameters:
#'
#' - `date`: Today's date.
#' - `wd`: Working directory.
#' - `sessionInfo`: R session information.
#'
#' @rdname prepareSE
#' @name prepareSE
#'
#' @param object Object supporting dimensions ([base::dim()]), or a list
#'   containing valid objects. For NGS experiments, a counts matrix is
#'   recommended, and can be passed in either dense (`matrix`) or sparse
#'   (`dgCMatrix`, `dgTMatrix`) format. Multiple matrices can be supplied as a
#'   list, as long as they all have the same dimensions. List object can be
#'   supplied as either class `list` or `SimpleList`.
#' @param colData **Required**. Object describing assay matrix columns.
#'   Must support [base::dim()].
#' @param rowData **Required**. Object describing assay matrix rows.
#'   Must support [base::dim()].
#' @param metadata *Optional*. Metadata list.
#'
#' @seealso
#' - [SummarizedExperiment::SummarizedExperiment].
#' - [base::Sys.Date()].
#' - [base::getwd()].
#' - [utils::sessionInfo()].
#'
#' @return [SummarizedExperiment].
#'
#' @examples
#' mat <- mtcars %>%
#'     .[c("Mazda RX4", "Datsun 710"), ] %>%
#'     .[, c("mpg", "gear")] %>%
#'     as.matrix
#' colData <- data.frame(
#'     description = c("Miles per gallon", "Number of gears"),
#'     abbreviation = c(TRUE, FALSE),
#'     row.names = colnames(mat))
#' rowData <- data.frame(
#'     manufacturer = c("Mazda", "Datsun"),
#'     model_number = c("RX4", "710"),
#'     row.names = rownames(mat))
#' prepareSE(mat, colData, rowData)
NULL



# Constructors ====
.prepareSE <- function(
    object,
    colData,
    rowData,
    metadata = NULL) {
    message("Preparing SummarizedExperiment")

    # Assays ====
    assays <- as(object, "SimpleList")
    assay <- assays[[1L]]
    if (is.null(dim(assay))) {
        stop("Assay object must support 'dim()'", call. = FALSE)
    }

    # colData ====
    if (is.null(dim(colData))) {
        stop("colData must support 'dim()'", call. = FALSE)
    }
    # Ensure `tibble` class coercion to `data.frame`
    if (is_tibble(colData)) {
        colData <- as.data.frame(colData)
    }
    # Attempt to use the first column for rownames, if unset
    if (is.data.frame(colData) & !has_rownames(colData)) {
        rownames(colData) <- colData[, 1L]
    }
    colData <- colData[colnames(assay), , drop = FALSE]
    rownames(colData) <- colnames(assay)
    colData <- as(colData, "DataFrame")

    # rowData ====
    if (is.null(dim(rowData))) {
        stop("rowData must support 'dim()'", call. = FALSE)
    }
    # Ensure `tibble` class coercion to `data.frame`
    if (is_tibble(rowData)) {
        rowData <- as.data.frame(rowData)
    }
    # Attempt to use the first column for rownames, if unset
    if (is.data.frame(rowData) & !has_rownames(rowData)) {
        rownames(rowData) <- rowData[, 1L]
    }
    rowData <- rowData[rownames(assay), , drop = FALSE]
    rownames(rowData) <- rownames(assay)
    rowData <- as(rowData, "DataFrame")

    # Check for name assignment problems
    if (any(duplicated(rownames(colData)))) {
        stop("Non-unique rownames in colData", call. = FALSE)
    }
    if (any(duplicated(rownames(rowData)))) {
        stop("Non-unique rownames in rowData", call. = FALSE)
    }
    if (!identical(colnames(assay), rownames(colData))) {
        stop("colData rowname mismatch", call. = FALSE)
    }
    if (!identical(rownames(assay), rownames(rowData))) {
        stop("rowData rowname mismatch", call. = FALSE)
    }

    # Metadata
    if (is.null(metadata)) {
        metadata <- SimpleList()
    } else {
        if (!any(is(metadata, "list") | is(metadata, "SimpleList"))) {
            stop("metadata must be 'list' or 'SimpleList' class")
        }
        metadata <- as(metadata, "SimpleList")
    }
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- getwd()
    metadata[["sessionInfo"]] <- sessionInfo()

    # Check for retired Ensembl identifiers, which can happen when a more recent
    # annotable build is used than the genome build. If present, store these
    # identifiers in the metadata.
    if (!is.null(rowData[["ensgene"]])) {
        if (any(is.na(rowData[["ensgene"]]))) {
            metadata[["missingGenes"]] <- rowData %>%
                .[is.na(.[["ensgene"]]), , drop = FALSE] %>%
                rownames %>%
                sort
        }
    }

    SummarizedExperiment(
        assays,
        colData = colData,
        rowData = rowData,
        metadata = metadata)
}



# Methods ====
#' @rdname prepareSE
#' @export
setMethod("prepareSE", "list", .prepareSE)



#' @rdname prepareSE
#' @export
setMethod("prepareSE", "SimpleList", .prepareSE)



#' @rdname prepareSE
#' @export
setMethod("prepareSE", "ANY", function(
    object, colData, rowData, metadata = NULL) {
    .prepareSE(
        SimpleList(assay = object),
        colData,
        rowData,
        metadata)
})
