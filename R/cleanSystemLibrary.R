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
    stopifnot(length(syslib) == 1)

    # Subset packages in the system library
    system <- x[which(x[, "LibPath"] == syslib), ]
    stopifnot(!any(is.na(system[, "Priority"])))

    # Check for packages built against a different point release
    # e.g. 3.5.1
    version <- getRversion()
    stopifnot(grepl("^\\d\\.\\d\\.\\d$", version))
    # e.g. 3.5
    version <- gsub("\\.\\d$", "", version)
    stopifnot(all(grepl(
        pattern = paste0("^", version),
        x = system[, "Built"]
    )))

    TRUE
}
