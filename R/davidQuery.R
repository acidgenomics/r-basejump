#' RDAVIDWebService query
#'
#' @param email Registered email address
#' @param foregroundGenes Gene ID vector
#' @param backgroundGenes Gene ID vector
#' @param idType Gene identifier type
#'
#' @export
davidQuery <- function(email, foregroundGenes, backgroundGenes, idType) {
  # RDAVIDWebService requires a registered email address:
  # http://david.abcc.ncifcrf.gov/content.jsp?file=WS.html

  url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/"
  david <- RDAVIDWebService::DAVIDWebService$new(email = email, url = url)

  # Foreground list
  # The foreground list should be contained within the background list.
  fg <- RDAVIDWebService::addList(
    david,
    foregroundGenes,
    idType = idType,
    listName = "isClass",
    listType = "Gene"
  )

  # Background list
  bg <- RDAVIDWebService::addList(
    david,
    backgroundGenes,
    idType = idType,
    listName = "all",
    listType = "Background"
  )

  # Inspect to see the proportion of genes recognized by DAVID.
  fg
  bg

  # Inspect "david" object to see the gene lists are correct.
  david

  # Specifiy annotation categories.
  # setAnnotationCategories(david, c("GOTERM_BP_ALL",
  #                                  "GOTERM_MF_ALL",
  #                                  "GOTERM_CC_ALL",
  #                                  "INTERPRO"))

  # Get functional annotation chart as R object.
  chart <- RDAVIDWebService::getFunctionalAnnotationChart(david)
  colnames(chart) <- camel(colnames(chart))

  # Get functional annotation clustering (limited to 3000 genes).
  #! cluster <- RDAVIDWebService::getClusterReport(david)
  #! assign("davidCluster", cluster, envir = .GlobalEnv)

  # Print functional annotation chart to file.
  RDAVIDWebService::getFunctionalAnnotationChartFile(david, "davidChart.tsv")

  # Print functional annotation clustering to file (limited to 3000 genes).
  RDAVIDWebService::getClusterReportFile(david, "davidCluster.tsv")

  return(chart)
}
