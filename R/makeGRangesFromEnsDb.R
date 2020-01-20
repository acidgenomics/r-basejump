#' Make GRanges from EnsDb object
#'
#' @details
#' Use specific `EnsDb` object as annotation source.
#' Alternatively, can pass in an EnsDb package name as a `character(1)`.
#'
#' @export
#' @include makeGRangesFromEnsembl.R
#' @note Updated 2020-01-20.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `GRanges`.
#'
#' @examples
#' if ("EnsDb.Hsapiens.v75" %in% rownames(installed.packages())) {
#'     x <- makeGRangesFromEnsDb("EnsDb.Hsapiens.v75")
#' }
makeGRangesFromEnsDb <- function(
    object,
    level,
    ignoreTxVersion = TRUE
) {
    level <- match.arg(level)
    cli_alert("Making {.var GRanges} from {.var EnsDb}.")
    userAttached <- .packages()
    ## Allow loading of EnsDb package, passed in as a character string.
    if (isString(object)) {
        package <- object
        requireNamespace(package = package, quietly = FALSE)
        object <- get(
            x = package,
            envir = asNamespace(package),
            inherits = FALSE
        )
    }
    assert(is(object, "EnsDb"))
    ## Get the genome build from the ensembldb metdata.
    m <- metadata(object)
    genomeBuild <- m[
        match(x = "genome_build", table = m[["name"]]),
        "value",
        drop = TRUE
    ]
    assert(isString(genomeBuild))
    ## Define the metadata to return.
    metadata <- list(
        organism = organism(object),
        genomeBuild = genomeBuild,
        ensemblRelease = as.integer(ensemblVersion(object)),
        ensembldb = metadata(object),
        level = level
    )
    cli_div(theme = list(body = list("margin-left" = 4L)))
    cli_dl(items = c(
        "Organism" = metadata[["organism"]],
        "Genome build" = metadata[["genomeBuild"]],
        "Ensembl release" = metadata[["ensemblRelease"]],
        "Level" = metadata[["level"]]
    ))
    cli_end()
    ## Always get gene-level annotations.
    genes <- genes(
        x = object,
        order.by = "gene_id",
        return.type = "GRanges"
    )
    if (level == "genes") {
        out <- genes
        metadata(out)[["level"]] <- genes
    }
    if (level == "transcripts") {
        transcripts <- transcripts(
            x = object,
            order.by = "tx_id",
            return.type = "GRanges"
        )
        ## Merge gene-level annotations into `mcols()`.
        out <- .mergeGenesIntoTranscripts(
            transcripts = transcripts,
            genes = genes
        )
    }
    ## Ensure ensembldb dependences get detached.
    .forceDetach(keep = userAttached)
    metadata(out) <- metadata
    out <- .makeGRanges(out, ignoreTxVersion = ignoreTxVersion)
    out
}

formals(makeGRangesFromEnsDb)[["level"]] <-
    formals(makeGRangesFromEnsembl)[["level"]]
