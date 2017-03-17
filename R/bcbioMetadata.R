#' Metadata for bcbio run
#'
#' @author Michael Steinbaugh
#' @keywords bcbio rnaseq
#'
#' @import dplyr
#' @import readr
#' @import stringr
#' @import tidyr
#'
#' @param project bcbio project
#' @param laneSplit Whether samples were split across flow cell lanes.
#' @param group Sample grouping. Used later in pipline for plot colors.
#'
#' @return Metadata data frame
#' @export
#'
#' @examples
#' \dontrun{
#' bcbioMetadata(project,
#'               group = "treatment",
#'               laneSplit = TRUE)
#' }
bcbioMetadata <- function(project,
                          group = "treatment",
                          laneSplit = FALSE) {
    metadata <- list.files(project$configDir,
                           pattern = ".csv",
                           full.names = TRUE) %>%
        readr::read_csv(.,
                        col_types = readr::cols()) %>%
        setNamesSnake %>%
        dplyr::rename_(.dots = c("name" = "samplename")) %>%
        dplyr::mutate_(.dots = setNames(list(group), "group"))

    # Lane splitting
    # Workflow used by Harvard Biopolymers Facility
    if (isTRUE(laneSplit)) {
        lane <- paste0("L", stringr::str_pad(1:4, 3, pad = "0"))
        metadata <- metadata %>%
            dplyr::group_by_(.dots = "name") %>%
            tidyr::expand_(~lane) %>%
            dplyr::left_join(metadata, by = "name") %>%
            dplyr::ungroup() %>%
            # NSE version
            # dplyr::mutate(description = paste(name, lane, sep = "_")) %>%
            dplyr::mutate_(
                .dots = setNames(
                    list(quote(paste(name, lane, sep = "_"))),
                    "description"
                ))
    }

    # Arrange the rows by description
    metadata <- metadata %>%
        dplyr::arrange_(.dots = "name") %>%
        setRownames("description")

    # Save binary
    dir.create("data", showWarnings = FALSE)
    save(metadata, file = "data/metadata.rda")

    # Write CSV
    dir.create("results", showWarnings = FALSE)
    write.csv(metadata, file = "results/metadata.csv")

    printTable(metadata,
               caption = "Sample metadata") %>% print

    return(metadata)
}
