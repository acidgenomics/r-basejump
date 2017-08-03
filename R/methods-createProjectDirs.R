#' Create Project Directory Structure
#'
#' Create necessary directory structure for an
#' [RMarkdown](http://rmarkdown.rstudio.com) report in
#' [RStudio](https://www.rstudio.com).
#'
#' @rdname createProjectDirs
#'
#' @return No value.
#' @export
setMethod("createProjectDirs", "missing", function() {
    localDirs <- c("data", "figures", "meta", "results")
    lapply(seq_along(localDirs), function(a) {
        dir.create(localDirs[[a]], showWarnings = FALSE)
    }) %>%
        invisible
})
