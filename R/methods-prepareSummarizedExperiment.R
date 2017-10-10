#' Prepare `SummarizedExperiment()`
#'
#' This is a utility wrapper for `SummarizedExperiment()` that provides
#' automatic subsetting for `colData` and `rowData`.
#'
#' This function also provides automatic metadata slotting of multiple useful
#' environment parameters:
#'
#' - `date`: Today's date.
#' - `wd`: Working directory.
#' - `utilsSessionInfo`: [utils::sessionInfo()] return.
#' - `devtoolsSessionInfo`: [devtools::session_info()] return.
#'
#' @rdname prepareSummarizedExperiment
#' @name prepareSummarizedExperiment
#' @family bcbio Utilities
#' @keywords internal
#'
#' @param assays List containing RNA-seq count matrices with matching
#'   dimensions. Counts can be passed in either dense (`matrix`) or sparse
#'   (`dgCMatrix`, `dgTMatrix`) format.
#' @param rowData Object describing assay matrix rows. Must support
#'   [base::dim()].
#' @param colData Object describing assay matrix columns. Must support
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
#' @export
#'
#' @examples
#' mat <- matrix(
#'     seq(1L:16L),
#'     nrow = 4L,
#'     ncol = 4L,
#'     dimnames = list(
#'         c("gene_1", "gene_2", "gene_3", "gene_4"),
#'         c("sample_1", "sample_2", "sample_3", "sample_4")))
#' rowData <- data.frame(
#'     ensgene = c("Aaa", "Bbb", "Ccc", "Ddd"),
#'     biotype = c("coding", "coding", "coding", "pseudogene"),
#'     row.names = rownames(mat))
#' colData <- data.frame(
#'     genotype = c("wt", "wt", "ko", "ko"),
#'     age = c(3L, 6L, 3L, 6L),
#'     row.names = colnames(mat))
#' prepareSummarizedExperiment(
#'     assays = list(assay = mat),
#'     rowData = rowData,
#'     colData = colData)
NULL



# Constructors ====
.prepareSummarizedExperiment <- function(
    assays,
    rowData,
    colData,
    metadata = NULL) {
    # Assays ====
    assay <- assays[[1L]]
    if (is.null(dim(assay))) {
        stop("Assay object must support 'dim()'")
    }
    # Check for potential dimnames problems
    if (is.null(rownames(assay))) {
        stop("Assay missing rownames", call. = FALSE)
    }
    if (is.null(colnames(assay))) {
        stop("Assay missing colnames", call. = FALSE)
    }
    if (any(duplicated(rownames(assay)))) {
        stop("Non-unique rownames", call. = FALSE)
    }
    if (any(duplicated(colnames(assay)))) {
        stop("Non-unique colnames", call. = FALSE)
    }
    if (!identical(make.names(rownames(assay)), rownames(assay))) {
        stop(paste(
            "Rownames are not valid.",
            "See 'base::make.names()' for more information."
            ), call. = FALSE)
    }
    if (!identical(make.names(colnames(assay)), colnames(assay))) {
        stop(paste(
            "Colnames are not valid.",
            "See 'base::make.names()' for more information."
            ), call. = FALSE)
    }

    # rowData ====
    if (is.null(dim(rowData))) {
        stop("rowData must support 'dim()'", call. = FALSE)
    }
    rowData <- as.data.frame(rowData)
    # Handle tibble rownames
    if (!has_rownames(rowData) &
        "rowname" %in% colnames(rowData)) {
        rowData <- column_to_rownames(rowData)
    }
    if (!has_rownames(rowData)) {
        stop("rowData missing rownames", call. = FALSE)
    }
    if (!all(rownames(assay) %in% rownames(rowData))) {
        missing <- setdiff(rownames(assay), rownames(rowData))
        # Warn instead of stop here, for better handling of deprecated
        # gene identifiers
        warning(paste(
            "rowData mismatch with assay slot:",
            toString(missing),
            "These IDs are missing in the annotable."
            ), call. = FALSE)
    }
    rowData <- rowData %>%
        .[rownames(assay), , drop = FALSE] %>%
        set_rownames(rownames(assay)) %>%
        as("DataFrame")

    # colData ====
    if (is.null(dim(colData))) {
        stop("colData must support 'dim()'", call. = FALSE)
    }
    colData <- as.data.frame(colData)
    # Handle tibble rownames
    if (!has_rownames(colData) &
        "rowname" %in% colnames(colData)) {
        colData <- column_to_rownames(colData)
    }
    if (!has_rownames(colData)) {
        stop("colData missing rownames", call. = FALSE)
    }
    if (!all(colnames(assay) %in% rownames(colData))) {
        missing <- setdiff(colnames(assay), rownames(colData))
        stop(paste(
            "colData mismatch with assay slot:",
            toString(head(missing))
        ), call. = FALSE)
    }
    colData <- colData %>%
        .[colnames(assay), , drop = FALSE] %>%
        set_rownames(colnames(assay)) %>%
        as("DataFrame")

    # Metadata ====
    if (is.null(metadata)) {
        metadata <- list()
    } else {
        if (!any(is(metadata, "list") | is(metadata, "SimpleList"))) {
            stop("Metadata must be 'list' or 'SimpleList' class object",
                 call. = FALSE)
        }
    }
    metadata[["date"]] <- Sys.Date()
    metadata[["wd"]] <- getwd()
    metadata[["utilsSessionInfo"]] <- utils::sessionInfo()
    metadata[["devtoolsSessionInfo"]] <- devtools::session_info()

    # Check for retired Ensembl identifiers, which can happen when a more recent
    # annotable build is used than the genome build. If present, store these
    # identifiers in the metadata.
    if (!is.null(rowData[["ensgene"]])) {
        if (any(is.na(rowData[["ensgene"]]))) {
            metadata[["missingGenes"]] <- rowData %>%
                .[is.na(.[["ensgene"]]), , drop = FALSE] %>%
                rownames() %>%
                sort()
        }
    }

    message("Preparing SummarizedExperiment")
    SummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata)
}



# Methods ====
#' @rdname prepareSummarizedExperiment
#' @export
setMethod(
    "prepareSummarizedExperiment",
    signature = "list",
    definition = .prepareSummarizedExperiment)
