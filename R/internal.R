# Unordered list.
.li <- "  -"



# Prototype metadata ===========================================================
.prototypeMetadata <- list(
    version = packageVersion("basejump"),
    date = Sys.Date()
)

.hasPrototypeMetadata <- function(object) {
    isSubset(
        x = names(.prototypeMetadata),
        y = names(metadata(object))
    )
}



# Prototype genome metadata ====================================================
.prototypeGenomeMetadata <- c(
    .prototypeMetadata,
    list(
        organism = character(),
        genomeBuild = character(),
        ensemblRelease = integer()
    )
)

.genomeMetadata <- function(object) {
    metadata <- metadata(object)
    prototypeMetadata <- .prototypeGenomeMetadata %>%
        .[setdiff(names(.), names(metadata))]
    c(prototypeMetadata, metadata)
}

.hasGenomeMetadata <- function(object) {
    all(genomeMetadataNames %in% names(metadata(object)))
}

.assertHasGenomeMetadata <- function(object) {
    if (!isTRUE(.hasGenomeMetadata(object))) {
        stop(paste0(
            "Object does not contain genome information.\n",
            "Requires: ", toString(genomeMetadataNames)
        ))
    }
    assert(.hasPrototypeMetadata(object))
}
