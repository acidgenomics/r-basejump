#' Check for Clean System Library
#'
#' Determine whether a user has installed packages into the system library.
#'
#' @author Michael Steinbaugh
#'
#' @return `logical`. Is system library clean?
#'
#' @examples
#' cleanSystemLibrary()
cleanSystemLibrary <- function() {
    x <- installed.packages()

    # Subset information on base packages
    base <- x[which(x[, "Priority"] == "base"), ]

    # Expect a single system library
    syslib <- unique(base[, "LibPath"])
    stopifnot(length(syslib) == 1)

    # Now let's check for outdated packages in the system library
    system <- x[which(x[, "LibPath"] == syslib), ]
    rversion <- getRversion()
    outdated <- system[which(system[, "Built"] != rversion), ]
    if (length(outdated)) {
        warning(paste(
            "Build mismatch in system library:",
            toString(rownames(outdated))
        ))
        FALSE
    } else {
        TRUE
    }
}
