#' Package into a `SummarizedExperiment`
#'
#' @author Michael Steinbaugh
#'
#' @param assays assays.
#' @param colData Sample metadata.
#' @param rowData [Ensembl](http://www.ensembl.org/) gene annotations.
#' @param metadata Custom metadata.
#'
#' @seealso [SummarizedExperiment::SummarizedExperiment].
#'
#' @return [SummarizedExperiment].
#' @export
packageSE <- function(
    assays,
    colData,
    rowData,
    metadata = NULL) {
    message("Packaging SummarizedExperiment")
    assays <- as(assays, "SimpleList")
    assay <- assays[[1L]]
    colData <- as.data.frame(colData)
    if (!has_rownames(colData)) {
        # Attempt to use the first column for rownames, if unset
        colData <- colData %>% set_rownames(.[[1L]])
    }
    colData <- colData %>%
        .[colnames(assay), , drop = FALSE] %>%
        set_rownames(colnames(assay)) %>%
        as("DataFrame")
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
        stop("Non-unique rownames in colData")
    }
    if (any(duplicated(rownames(rowData)))) {
        stop("Non-unique rownames in rowData")
    }
    if (!identical(colnames(assay), rownames(colData))) {
        stop("colData rowname mismatch")
    }
    if (!identical(rownames(assay), rownames(rowData))) {
        stop("rowData rowname mismatch")
    }

    # Metadata
    if (is.null(metadata)) {
        metadata <- SimpleList()
    } else {
        metadata <- as(metadata, "SimpleList")
    }
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- getwd()
    metadata[["hpc"]] <- detect_hpc()
    metadata[["session_info"]] <- sessionInfo()
    # Check for retired Ensembl identifiers, which can happen when a more recent
    # annotable build is used than the genome build. If present, store these
    # identifiers in the metadata.
    if (any(is.na(rowData[["ensgene"]]))) {
        metadata[["missing_ensgene"]] <- rowData %>%
            as_tibble %>%
            filter(is.na(.data[["ensgene"]])) %>%
            pull("rowname") %>%
            sort
    }

    SummarizedExperiment(
        assays,
        colData = colData,
        rowData = rowData,
        metadata = metadata)
}
