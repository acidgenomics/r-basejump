#' Package into a [SummarizedExperiment]
#'
#' @author Michael Steinbaugh
#'
#' @param assays assays.
#' @param colData Sample metadata.
#' @param rowData [Ensembl](http://www.ensembl.org/) gene annotations.
#' @param metadata Custom metadata.
#'
#' @return [SummarizedExperiment].
packageSE <- function(
    assays,
    colData,
    rowData,
    metadata = NULL) {
    message("Packaging SummarizedExperiment")
    assays <- as(assays, "SimpleList")
    assay <- assays[[1L]]
    colData <- colData %>%
        as.data.frame %>%
        set_rownames(.[[1L]]) %>%
        .[colnames(assay), , drop = FALSE] %>%
        set_rownames(colnames(assay)) %>%
        as("DataFrame")
    rowData <- rowData %>%
        as.data.frame %>%
        set_rownames(.[[1L]]) %>%
        .[rownames(assay), , drop = FALSE] %>%
        set_rownames(rownames(assay)) %>%
        as("DataFrame")

    # Check for assay name mismatches
    if (!identical(colnames(assay), rownames(colData))) {
        stop("Colname mismatch")
    }
    if (!identical(rownames(assay), rownames(rowData))) {
        stop("Rowname mismatch")
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
        metadata[["retired_ensgene"]] <- rowData %>%
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
