sam_exec <- function() {
  install.packages(c("samr", "matrixStats", "GSA", "shiny", "shinyFiles", "openxlsx"))
  source("http://bioconductor.org/biocLite.R")
  biocLite("impute")
  library(shiny)
  library(shinyFiles)
  runGitHub("SAM", "MikeJSeo")
}
