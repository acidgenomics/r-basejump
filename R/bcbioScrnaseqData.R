#' Import scRNA-Seq data from a bcbio project
#'
#' @author Michael Steinbaugh
#'
#' @import readr
#' @importFrom Matrix readMM
#'
#' @param project bcbio project
#'
#' @return List of scRNA-Seq data
#' @export
#'
#' @examples
#' \dontrun{
#' bcbioTagcounts(project)
#' }
bcbioScrnaseqData <- function(project) {
    tagcounts.mtx <- file.path(project$projectDir,
                               "tagcounts.mtx") %>%
        Matrix::readMM(.)
    rownames(tagcounts.mtx) <- file.path(project$projectDir,
                                         "tagcounts.mtx.rownames") %>%
        readr::read_lines(.)
    colnames(tagcounts.mtx) <- file.path(project$projectDir,
                                         "tagcounts.mtx.colnames") %>%
        readr::read_lines(.)
    list(tagcounts.mtx = tagcounts.mtx) %>%
        return
}
