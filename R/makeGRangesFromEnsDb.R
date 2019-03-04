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

    if (level == "genes") {
        gr <- genes(
            x = object,
            order.by = "gene_id",
            return.type = "GRanges"
        )
    } else if (level == "transcripts") {
        tx <- transcripts(
            x = object,
            order.by = "tx_id",
            return.type = "GRanges"
        )

        # Get additional mcols of interest from gene annotations.
        gene <- genes(
            x = object,
            order.by = "gene_id",
            return.type = "GRanges"
        )

        # Join the transcript- and gene-level annotations.
        txData <- mcols(tx)
        geneData <- mcols(gene)
        # Use BiocTibble left_join DataFrame method here.
        data <- left_join(
            x = as_tibble(txData, rownames = "rowname"),
            y = as_tibble(geneData, rownames = NULL),
            by = "gene_id"
        )
        assert(identical(x = txData[["tx_id"]], y = data[["tx_id"]]))

        # Now we can slot back into the transcript mcols.
        mcols(tx) <- as(data, "DataFrame")
        gr <- tx
    }

    metadata(gr) <- metadata
    .forceDetach(keep = userAttached)
    .makeGRanges(gr)
}

formals(makeGRangesFromEnsDb)[["level"]] <-
    formals(makeGRangesFromEnsembl)[["level"]]
