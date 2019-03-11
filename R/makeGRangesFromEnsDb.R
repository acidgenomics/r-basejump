#' Make `GRanges` from `EnsDb` object
#'
#' @details
#' Use specific `EnsDb` object as annotation source.
#' Alternatively, can pass in an EnsDb package name as a `character(1)`.
#'
#' @include makeGRangesFromEnsembl.R
#'
#' @inheritParams params
#' @export
#'
#' @examples
#' if ("EnsDb.Hsapiens.v75" %in% rownames(installed.packages())) {
#'     x <- makeGRangesFromEnsDb("EnsDb.Hsapiens.v75")
#' }
makeGRangesFromEnsDb <- function(object, level) {
    level <- match.arg(level)

    message("Making GRanges from EnsDb object.")
    userAttached <- .packages()

    # Allow loading of EnsDb package, passed in as a character string.
    if (isString(object)) {
        package <- object
        requireNamespace(package = package)
        object <- get(
            x = package,
            envir = asNamespace(package),
            inherits = FALSE
        )
    }
    assert(is(object, "EnsDb"))

    # Get the genome build from the ensembldb metdata.
    genomeBuild <- metadata(object) %>%
        as_tibble() %>%
        filter(!!sym("name") == "genome_build") %>%
        pull("value")
    assert(isString(genomeBuild))

    # Define the metadata to return.
    metadata <- list(
        organism = organism(object),
        genomeBuild = genomeBuild,
        ensemblRelease = as.integer(ensemblVersion(object)),
        ensembldb = metadata(object),
        level = level
    )

    message(paste(
        paste(.li, "Organism:", metadata[["organism"]]),
        paste(.li, "Genome Build:", metadata[["genomeBuild"]]),
        paste(.li, "Ensembl Release:", metadata[["ensemblRelease"]]),
        paste(.li, "Level:", metadata[["level"]]),
        sep = "\n"
    ))

    # Always get gene-level annotations.
    genes <- genes(object, order.by = "gene_id", return.type = "GRanges")
    if (level == "genes") {
        out <- genes
        metadata(out)[["level"]] <- genes
    }

    if (level == "transcripts") {
        transcripts <-
            transcripts(object, order.by = "tx_id", return.type = "GRanges")
        # Merge gene-level annotations into `mcols()`.
        out <- .mergeGenesIntoTranscripts(
            transcripts = transcripts,
            genes = genes
        )
    }

    # Ensure ensembldb dependences get detached.
    .forceDetach(keep = userAttached)

    metadata(out) <- metadata
    .makeGRanges(out)
}

formals(makeGRangesFromEnsDb)[["level"]] <-
    formals(makeGRangesFromEnsembl)[["level"]]
