#' Gene set enrichment analysis (GSEA) with DAVID
#'
#' @param gene Gene identifier vector.
#' @param background Background identifier vector.
#' @param format Identifier format (see DAVID website).
#' @param file Export to file (TRUE/FALSE).
#'
#' @return List of DAVID chart and cluster
#' @export
gseaDavid <- function(gene,
                      background,
                      format,
                      file = FALSE) {
    if (is.null(gene)) {
        stop("Gene vector is required.")
    }
    if (is.null(background)) {
        stop("A background correction vector is required.")
    }
    if (is.null(format)) {
        stop("Identifier format must be specified.")
    }
    if (is.null(getOption("email"))) {
        stop("An email must be specified in options().")
    }
    david <- RDAVIDWebService::DAVIDWebService$new(email = getOption("email"),
                                                   url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    gene <- RDAVIDWebService::addList(david, gene,
                                      idType = format,
                                      listName = "Gene",
                                      listType = "Gene")
    background <- RDAVIDWebService::addList(david, background,
                                            idType = format,
                                            listName = "Background",
                                            listType = "Background")

    if (isTRUE(file)) {
        RDAVIDWebService::getClusterReportFile(david, fileName = "davidClusterReport.tsv")
        RDAVIDWebService::getFunctionalAnnotationChartFile(david, fileName = "davidFunctionalAnnotationChart.tsv")
        RDAVIDWebService::getFunctionalAnnotationTableFile(david, fileName = "davidFunctionalAnnotationTable.tsv")
        RDAVIDWebService::getGeneListReportFile(david, fileName = "davidGeneListReportFile.tsv")
    }

    list <- list(annotationSummary = getAnnotationSummary(david),
                 clusterReport = getClusterReport(david),
                 functionalAnnotationChart = getFunctionalAnnotationChart(david),
                 functionalAnnotationTable = getFunctionalAnnotationTable(david),
                 geneCategoriesReport = getGeneCategoriesReport(david),
                 geneListReport = getGeneListReport(david))
    return(list)
}
