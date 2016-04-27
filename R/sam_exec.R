sam_exec <- function() {
  manage_github("impute")
  manage_cran(c("samr",
                "matrixStats",
                "GSA",
                "shiny",
                "shinyFiles",
                "openxlsx"))
  runGitHub("SAM", "MikeJSeo")
}
