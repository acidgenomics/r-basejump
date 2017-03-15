#' Load a remote bcbio project
#'
#' We recommend loading the \code{mountDir} as a connection over \code{sshfs}.
#'
#' @param researcher Researcher (primary investigator)
#' @param project Project name
#' @param workflow bcbio workflow template
#' @param mountDir SSH mount directory. Defaults to \code{bcbio} group share.
#'
#' @return \code{list} with directory paths
#' @export
#'
#' @examples
#' bcbioProject(researcher = "joe_smith",
#'              project = "gene_ko_rnaseq",
#'              workflow = "illumina_rnaseq",
#'              dirCreate = FALSE)
bcbioProject <- function(researcher,
                         project,
                         workflow,
                         mountDir = file.path("~",
                                              "Orchestra",
                                              "bcbio",
                                              "PIs"),
                         dirCreate = TRUE) {

    # `mountDir` defaults to Orchestra connection over `sshfs`
    rootDir <- file.path(mountDir,
                         researcher,
                         project) %>%
        normalizePath
    if (!length(dir(rootDir))) {
        stop("Project directory failed to load.")
    }

    # `bcbio-nextgen` run
    workflowDir <- file.path(rootDir, workflow)
    configDir <- file.path(workflowDir, "config")
    finalDir <- file.path(workflowDir, "final")

    # Default naming scheme is `illumina_rnaseq/final/YYYY-MM-DD_illumina_rnaseq`
    summaryDir <- file.path(finalDir) %>%
        dir(full.names = TRUE) %>%
        .[grepl(paste0("/\\d{4}-\\d{2}-\\d{2}_", workflow, "$"), .)]

    # Create directories, if desired
    if (isTRUE(dirCreate)) {
        dir.create("data", showWarnings = FALSE)
        dir.create("results", showWarnings = FALSE)
    }

    list(rootDir = rootDir,
         workflowDir = workflowDir,
         configDir = configDir,
         finalDir = finalDir,
         summaryDir = summaryDir) %>% return
}
