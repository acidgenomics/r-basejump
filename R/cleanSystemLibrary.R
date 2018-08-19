#' Check for Clean System Library
#'
#' Determine whether a user has installed packages into the system library.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @return `logical`. Is system library clean?
#' @export
#'
#' @examples
#' cleanSystemLibrary()
cleanSystemLibrary <- function() {
    x <- installed.packages()

    # Subset information on base packages
    base <- x[which(x[, "Priority"] == "base"), ]

    # Expect a single system library
    syslib <- unique(base[, "LibPath"])
    if (!identical(length(syslib), 1L)) {
        message("Detected multiple system libraries")
        return(FALSE)
    }

    # Subset packages in the system library
    system <- x[which(x[, "LibPath"] == syslib), ]
    if (any(is.na(system[, "Priority"]))) {
        message("Detected user-installed packages in system library")
        return(FALSE)
    }

    # Check for packages built against a different point release
    # e.g. 3.5.1
    version <- getRversion()
    stopifnot(grepl("^\\d\\.\\d\\.\\d$", version))
    # e.g. 3.5
    version <- gsub("\\.\\d$", "", version)
    if (!all(grepl(
        pattern = paste0("^", version),
        x = system[, "Built"]
    ))) {
        messages("Detected packages built against a different release")
        return(FALSE)
    }

    TRUE
}
