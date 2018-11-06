# cleanSystemLibrary ===========================================================
# We're checking for a logical in code coverage, but Travis CI will return
# FALSE here, and it's hard to test some of these conditions.
# nocov start
#' Check for Clean System Library
#'
#' Determine whether a user has installed packages into the system library.
#'
#' @return `logical`. Is system library clean?
#' @export
#'
#' @examples
#' cleanSystemLibrary()
cleanSystemLibrary <- function() {
    x <- installed.packages()

    # Subset information on base packages.
    base <- x[which(x[, "Priority"] == "base"), ]

    # Expect a single system library.
    syslib <- unique(base[, "LibPath"])
    if (!identical(length(syslib), 1L)) {
        message("Detected multiple system libraries")
        return(FALSE)
    }

    # Subset packages in the system library.
    system <- x[which(x[, "LibPath"] == syslib), ]
    if (any(is.na(system[, "Priority"]))) {
        message("Detected user-installed packages in system library")
        return(FALSE)
    }

    # Check for packages built against a different point release.
    # (e.g. 3.5.1)
    version <- getRversion()
    assert_that(grepl("^\\d\\.\\d\\.\\d$", version))
    # (e.g. 3.5)
    version <- gsub("\\.\\d$", "", version)
    if (!all(grepl(
        pattern = paste0("^", version),
        x = system[, "Built"]
    ))) {
        message("Detected packages built against a different release")
        return(FALSE)
    }

    TRUE
}
# nocov end



# detectHPC ====================================================================
#' Detect HPC Environment
#'
#' Detect if R is running on a high-performance computing (HPC) cluster.
#'
#' @note Currently supports detection of
#'   [HMS Orchestra](https://rc.hms.harvard.edu/#orchestra).
#'
#' @export
#'
#' @return `string` or `boolean`. Scheduler name if HPC is detected (e.g. LSF,
#'   SLURM), otherwise `FALSE`.
#'
#' @seealso
#' - `Sys.getenv()`.
#' - `Sys.info()`.
#' - `R.version`.
#' - `.Platform`.
#'
#' @examples
#' detectHPC()
detectHPC <- function() {
    if (!identical(Sys.getenv("LSF_ENVDIR"), "")) {
        "LSF"
    } else if (!identical(Sys.getenv("SLURM_CONF"), "")) {
        "SLURM"
    } else {
        FALSE
    }
}



# multiassignAsEnvir ===========================================================
#' Assign Multiple Objects as an Environment
#'
#' @export
#'
#' @inheritParams dots
#' @param envirName `string`. Name of the new `environment` to create.
#' @param parentFrame `environment`. Parent `environment` where to assign the
#'   new `environment`, specified by `envirName` argument.
#'
#' @return `character`. Object names defined in the new `environment`.
#'
#' @examples
#' data(rse, sce, package = "basejump.data")
#' multiassignAsEnvir(rse, sce, envirName = "example")
#' class(example)
#' ls(example)
multiassignAsEnvir <- function(
    ...,
    envirName,
    parentFrame = parent.frame()
) {
    dots <- dots(...)
    assert_is_list(dots)
    names <- dots(..., character = TRUE)
    assert_is_character(names)
    assert_is_a_string(envirName)
    assert_is_environment(parentFrame)

    envir <- new.env(parent = parentFrame)
    invisible(lapply(
        X = seq_along(dots),
        FUN = function(a) {
            assign(
                x = names[[a]],
                value = eval(expr = dots[[a]], envir = parentFrame),
                envir = envir
            )
        }
    ))

    message(paste0("Assigning ", toString(names), " as ", envirName, "."))
    assign(envirName, value = envir, envir = parentFrame)

    invisible(objects(envir))
}
