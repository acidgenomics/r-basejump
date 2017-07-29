#' Package Data into a `SummarizedExperiment`
#'
#' This is a utility wrapper for `SummarizedExperiment()` that provides
#' automatic subsetting for `colData` and `rowData`.
#'
#' `packageSE()` also provides automatic metadata slotting of multiple useful
#' environment parameters:
#'
#' - `date`: Today's date.
#' - `wd`: Working directory.
#' - `hpc`: High-performance computing cluster detection.
#' - `sessionInfo`: R session information.
#'
#' @author Michael Steinbaugh
#'
#' @param assays Assays.
#' @param colData Column data.
#' @param rowData Row data.
#' @param metadata Metadata.
#'
#' @seealso
#' - [SummarizedExperiment::SummarizedExperiment].
#' - [base::Sys.Date()].
#' - [base::getwd()].
#' - [detectHPC()].
#' - [utils::sessionInfo()].
#'
#' @return [SummarizedExperiment].
#' @export
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
#' packageSE(mat, colData, rowData)
packageSE <- function(
    assays,
    colData,
    rowData,
    metadata = NULL) {
    message("Packaging SummarizedExperiment")
    # Assays ====
    if (!is.null(dim(assays))) {
        # Coerce matrix-like object to SimpleList
        assays <- SimpleList(assay = assays)
    } else if (any(is(assays, "list") | is(assays, "SimpleList"))) {
        assays <- as(assays, "SimpleList")
    } else {
        stop("Invalid assays object")
    }
    assay <- assays[[1L]]

    # colData ====
    if (!any(is(colData, "data.frame") | is(colData, "DataFrame"))) {
        stop("colData must be a data frame", call. = FALSE)
    }
    colData <- as.data.frame(colData)
    if (!has_rownames(colData)) {
        # Attempt to use the first column for rownames, if unset
        colData <- colData %>% set_rownames(.[[1L]])
    }
    colData <- colData %>%
        .[colnames(assay), , drop = FALSE] %>%
        set_rownames(colnames(assay)) %>%
        as("DataFrame")

    # rowData ====
    if (!any(is(rowData, "data.frame") | is(rowData, "DataFrame"))) {
        stop("rowData must be a data frame")
    }
    rowData <- as.data.frame(rowData)
    if (!has_rownames(rowData)) {
        # Attempt to use the first column for rownames, if unset
        rowData <- rowData %>% set_rownames(.[[1L]])
    }
    rowData <- rowData %>%
        .[rownames(assay), , drop = FALSE] %>%
        set_rownames(rownames(assay)) %>%
        as("DataFrame")

    # Check for name assignment problems
    if (any(duplicated(rownames(colData)))) {
        stop("Non-unique rownames in colData", call. = FALSE)
    }
    if (any(duplicated(rownames(rowData)))) {
        stop("Non-unique rownames in rowData", call. = FALSE)
    }
    if (!identical(colnames(assay), rownames(colData))) {
        stop("Unexpected colData rowname mismatch", call. = FALSE)
    }
    if (!identical(rownames(assay), rownames(rowData))) {
        stop("Unexpected rowData rowname mismatch", call. = FALSE)
    }

    # Metadata
    if (is.null(metadata)) {
        metadata <- SimpleList()
    } else {
        if (!any(is(metadata, "list") | is(metadata, "SimpleList"))) {
            stop("metadata must be `list` or `SimpleList` object")
        }
        metadata <- as(metadata, "SimpleList")
    }
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- getwd()
    metadata[["hpc"]] <- detectHPC()
    metadata[["sessionInfo"]] <- sessionInfo()

    # Check for retired Ensembl identifiers, which can happen when a more recent
    # annotable build is used than the genome build. If present, store these
    # identifiers in the metadata.
    if (!is.null(rowData[["ensgene"]])) {
        if (any(is.na(rowData[["ensgene"]]))) {
            metadata[["missing_ensgene"]] <- rowData %>%
                as_tibble %>%
                filter(is.na(.data[["ensgene"]])) %>%
                pull("rowname") %>%
                sort
        }
    }

    SummarizedExperiment(
        assays,
        colData = colData,
        rowData = rowData,
        metadata = metadata)
}
