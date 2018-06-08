##' make a Rmarkdown link, for use in 'asis' blocks
##'
##' @title
##' @param title title of link
##' @param link link
##' @param text optional description of link, placed after it with a colon
##' @return markdown-formatted link
##' @author Rory Kirchner
##' @example \dontrun{
##' ```{r, results='asis'}
##' outfile = "results/outfile.csv"
##' readr::write_csv(outfile)
##' markdownLink("DE results", outfile, "Out file")
##' ```
##' }
markdownLink = function(desc, link, text=NULL) {
  mdlink = paste0('[', desc, '](',link,')')
  if(!is.null(text)) {
    mdlink = paste0(mdlink, ': ', text)
  }
  cat(markdownlink)
}
