#' Check to see if packages are installed
#'
#' @author Michael Steinbaugh
#'
#' @importFrom utils installed.packages
#'
#' @keywords internal
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
