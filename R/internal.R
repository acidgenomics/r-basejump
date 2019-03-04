#' Force Detach Packages
#'
#' ensembldb will attach unwanted packages into the NAMESPACE, which can
#' conflict with tidyverse packages (e.g. dplyr).
#'
#' @noRd
.forceDetach <- function(keep = NULL) {
    detach <- setdiff(.packages(), keep)
    if (length(detach) > 0L) {
        invisible(lapply(
            X = detach,
            FUN = function(name) {
                if (name %in% .packages()) {
                    suppressWarnings(detach(
                        name = paste0("package:", name),
                        unload = TRUE,
                        force = TRUE,
                        character.only = TRUE
                    ))
                }
            }
        ))
    }
    assert(identical(.packages(), keep))
}



# Unordered list.
.li <- "  -"



.prototypeMetadata <- list(
    version = packageVersion("basejump"),
    date = Sys.Date()
)



.slotGenomeMetadata <- function(object) {
    metadata <- metadata(object)
    proto <- c(
        .prototypeMetadata,
        list(
            organism = character(),
            genomeBuild = character(),
            ensemblRelease = integer()
        )
    )
    proto <- proto[setdiff(names(proto), names(metadata))]
    c(proto, metadata)
}



# Slot organism into `metadata()`, if necessary.
.slotOrganism <- function(object) {
    if (is.null(metadata(object)[["organism"]])) {
        metadata(object)[["organism"]] <- tryCatch(
            expr = organism(object),
            error = function(e) character()
        )
    }
    object
}
