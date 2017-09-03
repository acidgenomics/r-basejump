#' Prepare SummarizedExperiment
#'
#' This is a utility wrapper for `SummarizedExperiment()` that provides
#' automatic subsetting for `colData` and `rowData`.
#'
#' `prepareSummarizedExperiment()` also provides automatic metadata slotting of
#' multiple useful environment parameters:
#'
#' - `date`: Today's date.
#' - `wd`: Working directory.
#' - `sessionInfo`: R session information.
#'
#' @rdname prepareSummarizedExperiment
#' @name prepareSummarizedExperiment
#'
#' @param object Object supporting dimensions ([base::dim()]), or a list
#'   containing valid objects. For NGS experiments, a counts matrix is
#'   recommended, and can be passed in either dense (`matrix`) or sparse
#'   (`dgCMatrix`, `dgTMatrix`) format. Multiple matrices can be supplied as a
#'   list, as long as they all have the same dimensions. List object can be
#'   supplied as either class `list` or `SimpleList`.
#' @param colData Object describing assay matrix columns. Must support
#'   [base::dim()].
#' @param rowData Object describing assay matrix rows. Must support
#'   [base::dim()].
#' @param metadata *Optional*. Metadata list.
#'
#' @seealso
#' - [SummarizedExperiment::SummarizedExperiment()].
#' - [base::Sys.Date()].
#' - [base::getwd()].
#' - [utils::sessionInfo()].
#'
#' @return [SummarizedExperiment].
#'
#' @examples
#' mat <- mtcars %>%
#'     snake(rownames = TRUE) %>%
#'     .[c("mazda_rx4", "datsun_710"), ] %>%
#'     .[, c("mpg", "gear")]
#' colData <- data.frame(
#'     description = c("Miles per gallon", "Number of gears"),
#'     abbreviation = c(TRUE, FALSE),
#'     row.names = colnames(mat))
#' rowData <- data.frame(
#'     manufacturer = c("Mazda", "Datsun"),
#'     model_number = c("RX4", "710"),
#'     row.names = rownames(mat))
#' prepareSummarizedExperiment(mat, colData, rowData)
NULL



# Constructors ====
.prepareSEFromAssay <- function(
    object,
    colData,
    rowData,
    metadata = NULL) {
    .prepareSEFromList(
        list(assay = object),
        colData = colData,
        rowData = rowData,
        metadata = metadata)
}



.prepareSEFromList <- function(
    object,
    colData,
    rowData,
    metadata = NULL) {
    # Assays ====
    assays <- as(object, "SimpleList")
    assay <- assays[[1L]]
    if (is.null(dim(assay))) {
        stop("Assay object must support 'dim()'")
    }

    # Check for potential dim problems
    if (any(duplicated(rownames(assay)))) {
        stop("Non-unique rownames")
    }
    if (any(duplicated(colnames(assay)))) {
        stop("Non-unique colnames")
    }
    if (!identical(make.names(rownames(assay)), rownames(assay))) {
        stop(paste("Row names are not valid.",
                   "See 'base::make.names()' for more information."))
    }
    if (!identical(make.names(colnames(assay)), colnames(assay))) {
        stop(paste("Column names are not valid.",
                   "See 'base::make.names()' for more information."))
    }

    # colData ====
    if (is.null(dim(colData))) {
        stop("colData must support 'dim()'")
    }
    colData <- as.data.frame(colData)
    # Handle tibble rownames
    if (!has_rownames(colData) &
        "rowname" %in% colnames(colData)) {
        colData <- column_to_rownames(colData)
    }
    if (!has_rownames(colData)) {
        stop("colData missing rownames")
    }
    if (!all(colnames(assay) %in% rownames(colData))) {
        missing <- setdiff(colnames(assay), rownames(colData))
        stop(paste(
            "colData mismatch with assay slot:",
            toString(head(missing))
        ))
    }
    colData <- colData %>%
        .[colnames(assay), , drop = FALSE] %>%
        set_rownames(colnames(assay)) %>%
        as("DataFrame")

    # rowData ====
    if (is.null(dim(rowData))) {
        stop("rowData must support 'dim()'")
    }
    rowData <- as.data.frame(rowData)
    # Handle tibble rownames
    if (!has_rownames(rowData) &
        "rowname" %in% colnames(rowData)) {
        rowData <- column_to_rownames(rowData)
    }
    if (!has_rownames(rowData)) {
        stop("rowData missing rownames")
    }
    if (!all(rownames(assay) %in% rownames(rowData))) {
        missing <- setdiff(rownames(assay), rownames(rowData))
        # Warn instead of stop here, for better handling of deprecated
        # gene identifiers
        warning(paste(
            "rowData mismatch with assay slot:",
            toString(head(missing))
        ))
    }
    rowData <- rowData %>%
        .[rownames(assay), , drop = FALSE] %>%
        set_rownames(rownames(assay)) %>%
        as("DataFrame")

    # Metadata
    if (is.null(metadata)) {
        metadata <- list()
    } else {
        if (!any(is(metadata, "list") | is(metadata, "SimpleList"))) {
            stop("metadata must be 'list' or 'SimpleList' class")
        }
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

    message("Preparing SummarizedExperiment")
    SummarizedExperiment(
        assays = assays,
        colData = colData,
        rowData = rowData,
        metadata = metadata)
}



# Methods ====
#' @rdname prepareSummarizedExperiment
#' @export
setMethod("prepareSummarizedExperiment", "ANY", .prepareSEFromAssay)



#' @rdname prepareSummarizedExperiment
#' @export
setMethod("prepareSummarizedExperiment", "data.frame", .prepareSEFromAssay)



#' @rdname prepareSummarizedExperiment
#' @export
setMethod("prepareSummarizedExperiment", "dgCMatrix", .prepareSEFromAssay)



#' @rdname prepareSummarizedExperiment
#' @export
setMethod("prepareSummarizedExperiment", "list", .prepareSEFromList)



#' @rdname prepareSummarizedExperiment
#' @export
setMethod("prepareSummarizedExperiment", "matrix", .prepareSEFromAssay)



#' @rdname prepareSummarizedExperiment
#' @export
setMethod("prepareSummarizedExperiment", "SimpleList", .prepareSEFromList)
