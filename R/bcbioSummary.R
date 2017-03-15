#' Import a bcbio project summary
#'
#' @author Michael Steinbaugh
#' @keywords bcbio rnaseq
#'
#' @import dplyr
#' @import readr
#'
#' @param project bcbio project
#'
#' @export
#' @examples
#' \dontrun{
#' bcbioSummary(project)
#' }
bcbioSummary <- function(project) {
    summary <- file.path(project$summaryDir, "project-summary.csv") %>%
        readr::read_csv(., col_types = readr::cols()) %>%
        setNamesSnake %>%
        # Remove NA only columns
        .[, colSums(!is.na(.)) > 0] %>%
        # Sort by description
        dplyr::select_(.dots = c("description",
                                 setdiff(sort(names(.)),
                                         "description"))) %>%
        dplyr::arrange_(.dots = "description") %>%
        setRownames("description")

    # Save binary
    dir.create("data", showWarnings = FALSE)
    save(summary, file = "data/summary.rda")

    # Write CSV
    dir.create("results", showWarnings = FALSE)
    write.csv(summary, file = "results/summary.csv")

    return(summary)
}
