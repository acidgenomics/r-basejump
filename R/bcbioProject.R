#' Load a remote bcbio project
#'
#' We recommend loading the \code{mountDir} as a connection over \code{sshfs}.
#'
#' @author Michael Steinbaugh
#' @keywords bcbio hpc sshfs
#'
#' @param workflow bcbio workflow template
#' @param mountDir SSH mount directory. Defaults to \code{bcbio} group share.
#' @param subdirCreate Create subdirectories
#'
#' @return List with directory paths
#' @export
#'
#' @examples
#' \dontrun{
#' bcbioProject(workflow = "illumina_rnaseq",
#'              subdirCreate = FALSE)
#' }
bcbioProject <-
    function(workflow,
             mountDir = file.path("~",
                                  # Local symlink to `sshfs` mount
                                  "Orchestra",
                                  # Remote symlink to `/n/data1/cores/bcbio`
                                  "bcbio",
                                  "PIs",
                                  # Researcher
                                  basename(dirname(getwd())),
                                  # Project
                                  basename(getwd())),
             subdirCreate = TRUE) {

    # `mountDir` defaults to Orchestra server connection over `sshfs`
    mountDir <- normalizePath(mountDir)
    if (!length(dir(mountDir))) {
        stop("Project directory failed to load.")
    }

    # `bcbio-nextgen` run
    workflowDir <- file.path(mountDir, workflow)
    configDir <- file.path(workflowDir, "config")
    finalDir <- file.path(workflowDir, "final")

    # Project naming scheme is `workflow/final/YYYY-MM-DD_workflow`
    projectDir <- file.path(finalDir) %>%
        dir(full.names = TRUE) %>%
        .[grepl(paste0("/\\d{4}-\\d{2}-\\d{2}_", workflow, "$"), .)]

    # Create local directories, if desired
    if (isTRUE(subdirCreate)) {
        dir.create("data", showWarnings = FALSE)
        dir.create("results", showWarnings = FALSE)
    }

    list(mountDir = mountDir,
         workflowDir = workflowDir,
         configDir = configDir,
         finalDir = finalDir,
         projectDir = projectDir) %>% return
}
