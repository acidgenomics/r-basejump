#' Import a bcbio project summary
#'
#' @author Michael Steinbaugh
#'
#' @import dplyr
#' @import readr
#'
#' @param project bcbio project
#' @param metadata bcbio project metadata
#'
#' @export
#' @examples
#' \dontrun{
#' bcbioSummary(project)
#' }
bcbioSummary <- function(project, metadata) {
    if (!is.data.frame(metadata)) {
        stop("A metadata data frame is required.")
    }
    summary <- file.path(project$projectDir, "project-summary.csv") %>%
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

    # Set the group, used for plots
    summary$group <- metadata[rownames(summary), "group"]

    return(summary)
}
