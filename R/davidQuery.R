#' RDAVIDWebService query
#'
#' @param email Registered email address
#' @param fg foregroundGenes identifier vector
#' @param bg backgroundGenes identifier vector
#' @param id Gene identifier type (see DAVID website)
#'
#' @return Functional annotation chart data frame
#' @export
davidQuery <- function(email, fg, bg, id, save = NULL) {
  # RDAVIDWebService requires a registered email address:
  # http://david.abcc.ncifcrf.gov/content.jsp?file=WS.html

  url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/"
  david <- RDAVIDWebService::DAVIDWebService$new(email = email, url = url)

  # Foreground list
  # The foreground list should be contained within the background list.
  fg <- RDAVIDWebService::addList(
    david,
    fg,
    id = id,
    listName = "isClass",
    listType = "Gene"
  )

  # Background list
  bg <- RDAVIDWebService::addList(
    david,
    bg,
    id = id,
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
  annotationChart <- RDAVIDWebService::getFunctionalAnnotationChart(david)
  colnames(annotationChart) <- camel(colnames(annotationChart))

  # Get functional annotation clustering (limited to 3000 genes).
  #! annotationCluster <- RDAVIDWebService::getClusterReport(david)
  #! assign("davidCluster", annotationCluster, envir = .GlobalEnv)

  # Print functional annotation chart to file.
  RDAVIDWebService::getFunctionalAnnotationChartFile(
    david,
    file.path(save, "davidChart.tsv")
    )

  # Print functional annotation clustering to file (limited to 3000 genes).
  RDAVIDWebService::getClusterReportFile(
    david,
    file.path(save, "davidCluster.tsv")
    )

  return(annotationChart)
}
