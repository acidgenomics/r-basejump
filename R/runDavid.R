#' DAVID enrichment analysis
#'
#' Wrapper function that performs gene set enrichment analysis (GSEA) with
#' RDAVIDWebService, using simplified input options.
#'
#' @author Michael Steinbaugh
#'
#' @import dplyr
#' @import readr
#' @importFrom RDAVIDWebService addList DAVIDWebService getAnnotationSummary
#'   getClusterReport getClusterReportFile getFunctionalAnnotationChart
#'   getFunctionalAnnotationChartFile getFunctionalAnnotationTable
#'   getFunctionalAnnotationTableFile getGeneCategoriesReport getGeneListReport
#'   getGeneListReportFile setTimeOut
#'
#' @param foreground Foreground identifiers
#' @param background Background identifiers
#' @param idType Identifier type (see DAVID website)
#' @param writeFiles Write files to disk (\code{TRUE/FALSE})
#' @param writePrefix Add an optional prefix to the file names
#' @param writeDir Directory to write the TSV files
#' @param count Minimum hit count
#' @param fdr False discovery rate cutoff (alpha)
#'
#' @return List of \code{RDAVIDWebService()} report objects
#' @export
runDavid <- function(foreground,
                     background = NULL,
                     idType = "ENSEMBL_GENE_ID",
                     writeFiles = TRUE,
                     writePrefix = NULL,
                     writeDir = "results/david",
                     count = 3,
                     fdr = 0.1) {
    if (is.null(getOption("email"))) {
        stop("no email found in options().
             can be saved globally in .Rprofile.")
    }
    if (is.null(foreground)) {
        stop("A foreground gene vector is required.")
    }
    if (is.null(idType) | length(idType) != 1) {
        stop("identifier string is required")
    }
    if (!is.numeric(count) | length(count) != 1 | count < 0) {
        stop("please specify the minimum count cutoff of gene hits per
             annotation as a single non-negative numeric")
    }
    if (!is.numeric(fdr) | length(fdr) != 1 | fdr < 0 | fdr > 1) {
        stop("please specify the false discovery rate (FDR) cutoff as a single
             numeric in the range of 0-1")
    }

    david <- DAVIDWebService$new(
        email = getOption("email"),
        url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/"
    )

    # Set a longer timeout (30000 default)
    setTimeOut(david, 200000)
    # getTimeOut(david)

    addList(david,
            foreground,
            idType = idType,
            listName = "Gene",
            listType = "Gene")

    # Set the background only for screens and microarrays
    # Use the organism default for RNA-Seq
    if (!is.null(background)) {
        addList(david,
                background,
                idType = idType,
                listName = "Background",
                listType = "Background")
    }

    # Generate the annotation chart with cutoffs applied
    cutoffChart <- getFunctionalAnnotationChart(david) %>% as.data.frame
    if (nrow(cutoffChart) > 0) {
        cutoffChart <- cutoffChart %>%
            setNamesSnake %>%
            .[, c("category",
                  "term",
                  "count",
                  "genes",
                  "pvalue",
                  "fdr")] %>%
            dplyr::rename_(.dots = c("p" = "pvalue")) %>%
            # FDR should be presented on 0-1 scale, not as a percentage
            dplyr::mutate_(.dots = setNames(list(~fdr / 100), "fdr")) %>%
            dplyr::arrange_(.dots = c("category", "fdr")) %>%
            .[.$count >= count, ] %>%
            .[.$fdr < fdr, ]
        # Set NULL if everything got filtered
        if (nrow(cutoffChart) == 0) {
            cutoffChart <- NULL
        }
    } else {
        cutoffChart <- NULL
    }

    # Save the TSV files to disk
    if (isTRUE(writeFiles)) {
        dir.create(writeDir,
                   recursive = TRUE,
                   showWarnings = FALSE)

        if (!is.null(writePrefix)) {
            prefix <- paste0(writePrefix, "_")
        } else {
            prefix <- ""
        }

        getClusterReportFile(
            david,
            fileName = file.path(writeDir,
                                 paste0(prefix,
                                        "cluster_report.tsv")))
        getFunctionalAnnotationChartFile(
            david,
            fileName = file.path(writeDir,
                                 paste0(prefix,
                                        "functional_annotation_chart.tsv")))
        getFunctionalAnnotationTableFile(
            david,
            fileName = file.path(writeDir,
                                 paste0(prefix,
                                        "functional_annotation_table.tsv")))
        getGeneListReportFile(
            david,
            fileName = file.path(writeDir,
                                 paste0(prefix,
                                        "gene_list_report_file.tsv")))

        if (!is.null(cutoffChart)) {
            readr::write_tsv(
                cutoffChart,
                file.path(writeDir,
                          paste0(prefix, "cutoff_chart.tsv"))
            )
        }
    }

    # Package DAVID output into a list
    return(list(
        annotationSummary = getAnnotationSummary(david),
        clusterReport = getClusterReport(david),
        cutoffChart = cutoffChart,
        functionalAnnotationChart = getFunctionalAnnotationChart(david),
        functionalAnnotationTable = getFunctionalAnnotationTable(david),
        geneCategoriesReport = getGeneCategoriesReport(david),
        geneListReport = getGeneListReport(david)
    ))
}
