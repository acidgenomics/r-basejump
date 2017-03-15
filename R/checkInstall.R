#' Check to see if packages are installed
#'
#' @importFrom utils installed.packages
#'
#' @param packages Character vector of packages
#'
#' @export
checkInstall <- function(packages) {
    not_installed <- setdiff(packages, rownames(installed.packages()))
    if (length(not_installed) > 0) {
        write(paste("The libraries", not_installed, "are not installed, aborting.",
                    sep = " "), stderr())
        stop()
    }
}
