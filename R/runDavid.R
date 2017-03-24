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
#' @param saveFiles Save files to disk (\code{TRUE/FALSE})
#' @param saveDir Directory where to save TSV files
#' @param count Minimum hit count
#' @param fdr False discovery rate cutoff (alpha)
#'
#' @return List of \code{RDAVIDWebService()} report objects
#' @export
runDavid <- function(foreground,
                  background = NULL,
                  idType = "ENSEMBL_GENE_ID",
                  saveFiles = TRUE,
                  saveDir = "results/david",
                  count = 3,
                  fdr = 0.1) {
    if (is.null(getOption("email"))) {
        stop("An email must be specified using options().
             We recommend globally saving options in `~/.Rprofile`.")
    }
    if (is.null(foreground)) {
        stop("A foreground gene vector is required.")
    }
    if (is.null(idType) | length(idType) != 1) {
        stop("An single identifier type must be specified.")
    }
    if (!is.numeric(count) | length(count) != 1 | count < 0) {
        stop("Please specify the minimum count cutoff of gene hits per
             annotation as a single non-negative numeric.")
    }
    if (!is.numeric(fdr) | length(fdr) != 1 | fdr < 0 | fdr > 1) {
        stop("Please specify the false discovery rate (FDR) cutoff as a single
             numeric in the range of 0-1.")
    }

    david <- RDAVIDWebService::DAVIDWebService$new(
        email = getOption("email"),
        url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/"
    )

    # Set a longer timeout (30000 default)
    RDAVIDWebService::setTimeOut(david, 200000)
    #` RDAVIDWebService::getTimeOut(david)

    RDAVIDWebService::addList(david,
                              foreground,
                              idType = idType,
                              listName = "Gene",
                              listType = "Gene")

    # Set the background only for screens and microarrays
    # Use the organism default for RNA-Seq
    if (!is.null(background)) {
        RDAVIDWebService::addList(david,
                                  background,
                                  idType = idType,
                                  listName = "Background",
                                  listType = "Background")
    }

    # Generate the annotation chart with cutoffs applied
    cutoffChart <- RDAVIDWebService::getFunctionalAnnotationChart(david) %>%
        as.data.frame(.) %>%
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

    # Save the TSV files to disk
    if (isTRUE(saveFiles)) {
        if (!is.null(saveDir)) {
            # Create the saveDir if necessary
            if (!dir.exists(saveDir)) {
                dir.create(saveDir, recursive = TRUE)
            }
        }
        RDAVIDWebService::getClusterReportFile(david, fileName = file.path(saveDir, "cluster_report.tsv"))
        RDAVIDWebService::getFunctionalAnnotationChartFile(david, fileName = file.path(saveDir, "functional_annotation_chart.tsv"))
        RDAVIDWebService::getFunctionalAnnotationTableFile(david, fileName = file.path(saveDir, "functional_annotation_table.tsv"))
        RDAVIDWebService::getGeneListReportFile(david, fileName = file.path(saveDir, "gene_list_report_file.tsv"))
        readr::write_tsv(cutoffChart, file.path(saveDir, "cutoff_chart.tsv"))
    }

    # Package DAVID output into a list
    list(annotationSummary = RDAVIDWebService::getAnnotationSummary(david),
         clusterReport = RDAVIDWebService::getClusterReport(david),
         cutoffChart = cutoffChart,
         functionalAnnotationChart = RDAVIDWebService::getFunctionalAnnotationChart(david),
         functionalAnnotationTable = RDAVIDWebService::getFunctionalAnnotationTable(david),
         geneCategoriesReport = RDAVIDWebService::getGeneCategoriesReport(david),
         geneListReport = RDAVIDWebService::getGeneListReport(david))
}
