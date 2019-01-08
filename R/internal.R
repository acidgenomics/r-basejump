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
