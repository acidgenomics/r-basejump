#' Make Genomic Ranges
#'
#' @name makeGRanges
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @section Ensembl annotations:
#'
#' Quickly obtain gene and transcript annotations from
#' [Ensembl](http://www.ensembl.org) using
#' [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub) and
#' [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' Simply specify the desired organism, using the full latin name. For example,
#' we can obtain human annotations with `Homo sapiens`. Optionally, specific
#' Ensembl genome builds (e.g. `GRCh38`) and release versions (e.g. `87`) are
#' supported.
#'
#' Under the hood, this function fetches annotations from AnnotationHub using
#' the ensembldb package. AnnotationHub supports versioned Ensembl releases,
#' back to version 87.
#'
#' Genome build: use "`GRCh38`" instead of "`hg38`" for the genome build, since
#' we're querying Ensembl and not UCSC.
#'
#' @section GTF/GFF file:
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2. We recommend
#' using a GTF file instead of a GFF3 file, if possible.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' Remote URLs and compressed files are supported.
#'
#' @section GRCh37 legacy annotations:
#'
#' [makeGRangesFromEnsembl()] supports the legacy *Homo sapiens* GRCh37 (release
#' 75) build by internally querying the
#' [EnsDb.Hsapiens.v75](https://doi.org/doi:10.18129/B9.bioc.EnsDb.Hsapiens.v75)
#' package. Alternatively, the corresponding GTF/GFF file can be loaded directly
#' from GENCODE or Ensembl.
#'
#' @section Broad class definitions:
#'
#' For gene and transcript annotations, a `broadClass` column is added, which
#' generalizes the gene types into a smaller number of semantically-meaningful
#' groups:
#'
#'   - `coding`.
#'   - `noncoding`.
#'   - `pseudo`.
#'   - `small`.
#'   - `decaying`.
#'   - `ig` (immunoglobulin).
#'   - `tcr` (T cell receptor).
#'   - `other`.
#'
#' @section annotable:
#'
#' [annotable()] is a legacy convenience function that calls
#' [makeGRangesFromEnsembl()] and returns a `tibble` instead of `GRanges`. Note
#' that `GRanges` also support coercion to a basic `data.frame` using
#' [as.data.frame()].
#'
#' @inheritParams general
#' @param level `string`. Return ranges as "`genes`" or "`transcripts`".
#' @param build `string` or `NULL`. Genome build assembly name (e.g.
#'   "`GRCh38`"). If set `NULL`, defaults to the most recent build available.
#' @param release `scalar integer` or `NULL`. Release version (e.g. `90`). If
#'   set `NULL`, defaults to the most recent release available.
#'
#' @return
#' - `makeGRangesFromEnsembl()`, `makeGRangesFromGFF()`: `GRanges`.
#' - `annotable()`: `tbl_df`.
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#' - [rtracklayer::import()].
#' - [GenomicFeatures::makeTxDbFromGFF()].
#'
#' @examples
#' # makeGRangesFromEnsembl ====
#' # Genes
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "genes")
#' summary(x)
#'
#' # Transcripts
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "transcripts")
#' summary(x)
#'
#' # makeGRangesFromGFF ====
#' file <- file.path(basejumpCacheURL, "example.gtf")
#'
#' # Genes
#' x <- makeGRangesFromGFF(file = file, level = "genes")
#' summary(x)
#'
#' # Transcripts
#' x <- makeGRangesFromGFF(file = file, level = "transcripts")
#' summary(x)
#'
#' # annotable ====
#' # Genes
#' x <- annotable("Homo sapiens")
#' glimpse(x)
#'
#' # Transcripts
#' x <- annotable("Homo sapiens", level = "transcripts")
#' glimpse(x)
NULL



#' @rdname makeGRanges
#' @export
makeGRangesFromEnsembl <- function(
    organism,
    level = c("genes", "transcripts"),
    build = NULL,
    release = NULL
) {
    assert_is_a_string(organism)
    # Standard organism query, if necessary.
    organism <- gsub("_", " ", makeNames(organism))
    level <- match.arg(level)
    assertIsAStringOrNULL(build)
    # Check for accidental UCSC input and stop, informing user.
    if (is_a_string(build)) {
        ucscCheck <- tryCatch(
            expr = convertUCSCBuildToEnsembl(build),
            error = function(e) NULL
        )
        if (has_length(ucscCheck)) {
            stop(paste(
                "UCSC build ID detected.",
                "Use Ensembl ID instead.\n",
                printString(ucscCheck)
            ))
        }
    }
    assertIsAnImplicitIntegerOrNULL(release)
    if (isAnImplicitInteger(release)) {
        # Note that ensembldb currently only supports >= 87.
        assert_all_are_positive(release)
        release <- as.integer(release)
    }

    # Ensure `select()` isn't masked by ensembldb and/or AnnotationDbi.
    userAttached <- .packages()

    # Fetch annotations from AnnotationHub/ensembldb ---------------------------
    # ah: AnnotationHub
    # edb: Ensembl database
    if (
        identical(tolower(organism), "homo sapiens") &&
        (
            identical(tolower(as.character(build)), "grch37") ||
            identical(release, 75L)
        )
    ) {
        # GRCh37 release 75
        ahid <- "EnsDb.Hsapiens.v75"
        if (requireNamespace(ahid, quietly = TRUE)) {
            edb <- get(ahid, envir = asNamespace(ahid), inherits = FALSE)
        } else {
            # nocov start
            stop(paste(
                "GRCh37 genome build requires the", ahid, "package."
            ))
            # nocov end
        }
    } else {
        # AnnotationHub --------------------------------------------------------
        # Connect to AnnotationHub. On a fresh install this will print a
        # txProgressBar to the console. We're using `capture.output()` here
        # to suppress the console output, since it's not very informative and
        # can cluster R Markdown reports.
        invisible(capture.output(
            ah <- suppressMessages(AnnotationHub())
        ))

        message(paste0(
            "Making GRanges from Ensembl with AnnotationHub ",
            packageVersion("AnnotationHub"),
            " (", snapshotDate(ah), ")..."
        ))

        # Use ensembldb annotations by default.
        rdataclass <- "EnsDb"

        # For legacy release requests, switch to newest version available.
        if (!is.null(release) && release < 87L) {
            warning(paste(
                "ensembldb currently only supports Ensembl releases >= 87.",
                "Switching to current release instead.",
                sep = "\n"
            ))
            release <- NULL
        }

        # Query AnnotationHub.
        ahs <- query(
            x = ah,
            pattern = c(
                "Ensembl",
                organism,
                build,
                release,
                rdataclass
            ),
            ignore.case = TRUE
        )

        # Get the AnnotationHub from the metadata columns.
        mcols <- mcols(ahs)

        # Abort if there's no match and working offline.
        if (
            !isTRUE(has_internet()) &&
            !nrow(mcols)
        ) {
            # nocov start
            stop("AnnotationHub requires an Internet connection for query.")
            # nocov end
        }

        # Ensure build matches, if specified.
        if (!is.null(build)) {
            assert_is_subset("genome", colnames(mcols))
            mcols <- mcols[mcols[["genome"]] %in% build, , drop = FALSE]
        }

        # Ensure release matches, or pick the latest one.
        if (!is.null(release)) {
            assert_is_subset("title", colnames(mcols))
            mcols <- mcols[
                grepl(paste("Ensembl", release), mcols[["title"]]),
                ,
                drop = FALSE
            ]
            assert_is_of_length(nrow(mcols), 1L)
        }

        if (!nrow(mcols)) {
            stop(paste(
                paste0(
                    "No ID matched on AnnotationHub ",
                    packageVersion("AnnotationHub"), "."
                ),
                paste(li, "Organism:", deparse(organism)),
                paste(li, "Build:", deparse(build)),
                paste(li, "Release:", deparse(release)),
                sep = "\n"
            ))
        }

        mcols <- tail(mcols, 1L)
        message(mcols[["title"]])
        ahid <- rownames(mcols)

        # This step will also output `txProgressBar()` on a fresh install. Using
        # `capture.output()` here again to suppress console output.
        # Additionally, it attaches ensembldb and other Bioconductor dependency
        # packages, which will mask some tidyverse functions (e.g. `select()`).
        invisible(capture.output(
            edb <- suppressMessages(ah[[ahid]])
        ))
    }

    # Get annotations from EnsDb -----------------------------------------------
    assert_is_all_of(edb, "EnsDb")

    # Get the genome build from the ensembldb metdata.
    build <- metadata(edb) %>%
        as_tibble() %>%
        filter(!!sym("name") == "genome_build") %>%
        pull("value")
    assert_is_a_string(build)

    # Ready to create our metadata list to stash inside the GRanges.
    metadata <- list(
        AnnotationHubID = ahid,
        organism = organism(edb),
        ensembldb = metadata(edb),
        build = build,
        release = ensemblVersion(edb),
        level = level,
        version = packageVersion("basejump"),
        date = Sys.Date()
    )

    message(paste(
        paste(li, "AnnotationHub:", deparse(metadata[["AnnotationHubID"]])),
        paste(li, "Organism:", deparse(metadata[["organism"]])),
        paste(li, "Build:", deparse(metadata[["build"]])),
        paste(li, "Release:", deparse(metadata[["release"]])),
        paste(li, "Level:", deparse(metadata[["level"]])),
        sep = "\n"
    ))

    if (level == "genes") {
        gr <- genes(
            x = edb,
            order.by = "gene_id",
            return.type = "GRanges"
        )
    } else if (level == "transcripts") {
        tx <- transcripts(
            x = edb,
            order.by = "tx_id",
            return.type = "GRanges"
        )

        # Get additional mcols of interest from gene annotations.
        gene <- genes(
            x = edb,
            order.by = "gene_id",
            return.type = "GRanges"
        )

        # Merge the data.
        txData <- mcols(tx)
        geneData <- mcols(gene)
        # FIXME Update to use `left_join()` here instead.
        mergeData <- merge(
            x = txData,
            y = geneData,
            by = "gene_id",
            all.x = TRUE,
            sort = FALSE
        )
        assert_are_set_equal(txData[["tx_id"]], mergeData[["tx_id"]])
        match <- match(x = txData[["tx_id"]], table = mergeData[["tx_id"]])
        stopifnot(!any(is.na(match)))
        mergeData <- mergeData[match, , drop = FALSE]
        assert_are_identical(txData[["tx_id"]], mergeData[["tx_id"]])

        # Now we can slot back into the transcript mcols.
        mcols(tx) <- mergeData
        gr <- tx
    }

    # Force detach -------------------------------------------------------------
    # ensembldb will attach unwanted packages into the NAMESPACE, which can
    # conflict with tidyverse.
    fxnAttached <- setdiff(.packages(), userAttached)
    invisible(lapply(
        X = fxnAttached,
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
    assert_are_identical(.packages(), userAttached)

    # Always stash the metadata.
    metadata(gr) <- metadata

    .makeGRanges(gr)
}



#' @rdname makeGRanges
#' @export
makeGRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts")
) {
    # Note that `import()` has assert checks for file (see below).
    level <- match.arg(level)

    # Stash the formals used into a metadata list, which we'll slot into
    # our GRanges at the end of the call.
    metadata <- list(
        file = file,
        level = level,
        version = packageVersion("basejump"),
        date = Sys.Date()
    )

    # Read GFF -----------------------------------------------------------------
    file <- localOrRemoteFile(file)
    gff <- import(file)
    assert_is_all_of(gff, "GRanges")
    gff <- camel(gff)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # nocov start
    if (
        source %in% c("FlyBase", "WormBase") &&
        type == "GFF"
    ) {
        stop(paste0(
            "Only GTF files are currently supported from ", source, "."
        ))
    }
    # nocov end

    # Always require `geneID` and `transcriptID` columns in file.
    assert_is_subset(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(gff))
    )

    # Rename `geneSymbol` to `geneName`.
    # This applies to FlyBase and WormBase annotations.
    colnames(mcols(gff)) <- gsub(
        pattern = "Symbol$",
        replacement = "Name",
        x = colnames(mcols(gff))
    )

    # Genes --------------------------------------------------------------------
    gn <- gff
    gn <- gn[!is.na(mcols(gn)[["geneID"]])]
    gn <- gn[is.na(mcols(gn)[["transcriptID"]])]
    if (type == "GFF") {
        # geneName
        assert_is_subset("name", colnames(mcols(gn)))
        mcols(gn)[["geneName"]] <- mcols(gn)[["name"]]
        mcols(gn)[["name"]] <- NULL
        # geneBiotype
        assert_is_subset("biotype", colnames(mcols(gn)))
        mcols(gn)[["geneBiotype"]] <- mcols(gn)[["biotype"]]
        mcols(gn)[["biotype"]] <- NULL
        # Remove extra columns
        mcols(gn)[["alias"]] <- NULL
        mcols(gn)[["id"]] <- NULL
        mcols(gn)[["parent"]] <- NULL
    }
    assert_has_no_duplicates(mcols(gn)[["geneID"]])
    names(gn) <- mcols(gn)[["geneID"]]
    gn <- gn[sort(names(gn))]

    # Stop on missing genes.
    assert_are_identical(
        x = names(gn),
        y = sort(unique(na.omit(mcols(gff)[["geneID"]])))
    )

    if (level == "genes") {
        message(paste(length(gn), "gene annotations detected."))
        gr <- gn
    }

    # Transcripts --------------------------------------------------------------
    if (level == "transcripts") {
        tx <- gff
        tx <- tx[!is.na(mcols(tx)[["transcriptID"]])]
        if (type == "GTF") {
            types <- c(
                "pseudogene",
                "rna",
                "transcript"
            )
            tx <- tx[grepl(
                pattern = paste(types, collapse = "|"),
                x = mcols(tx)[["type"]],
                ignore.case = TRUE
            )]
        } else if (type == "GFF") {
            # transcriptName
            assert_is_subset("name", colnames(mcols(tx)))
            mcols(tx)[["transcriptName"]] <- mcols(tx)[["name"]]
            mcols(tx)[["name"]] <- NULL
            # transcriptBiotype
            assert_is_subset("biotype", colnames(mcols(tx)))
            mcols(tx)[["transcriptBiotype"]] <- mcols(tx)[["biotype"]]
            mcols(tx)[["biotype"]] <- NULL
            # geneID
            assert_is_subset("parent", colnames(mcols(tx)))
            stopifnot(all(grepl("^gene:", mcols(tx)[["parent"]])))
            mcols(tx)[["geneID"]] <- as.character(mcols(tx)[["parent"]])
            mcols(tx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["geneID"]]
            )
            # Remove extra columns
            mcols(tx)[["alias"]] <- NULL
            mcols(tx)[["id"]] <- NULL
            mcols(tx)[["parent"]] <- NULL
        }
        assert_has_no_duplicates(mcols(tx)[["transcriptID"]])
        names(tx) <- mcols(tx)[["transcriptID"]]
        tx <- tx[sort(names(tx))]

        # Stop on missing transcripts.
        assert_are_identical(
            x = names(tx),
            y = sort(unique(na.omit(mcols(gff)[["transcriptID"]])))
        )

        message(paste(length(tx), "transcript annotations detected."))
        gr <- tx

        # Merge the gene-level annotations (`geneName`, `geneBiotype`).
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(gr))
        )
        if (has_length(geneCols)) {
            geneCols <- c("geneID", geneCols)
            merge <- merge(
                x = mcols(gr),
                y = mcols(gn)[, geneCols],
                all.x = TRUE,
                by = "geneID"
            )
            rownames(merge) <- merge[["transcriptID"]]
            merge <- merge[sort(rownames(merge)), ]
            assert_are_identical(
                x = mcols(gr)[["transcriptID"]],
                y = merge[["transcriptID"]]
            )
            mcols(gr) <- merge
        }
    }

    # Stash our metadata into the GRanges.
    metadata(gr) <- metadata

    .makeGRanges(gr)
}



.makeGRanges <- function(object) {
    assert_is_all_of(object, "GRanges")
    assert_has_names(object)

    # Standardize the metadata columns.
    message("Standardizing the metadata columns...")
    mcols <- mcols(object)
    # Sanitize to camel case
    mcols <- camel(mcols)
    # Ensure "ID" is always capitalized (e.g. "entrezid").
    colnames(mcols) <- gsub("id$", "ID", colnames(mcols))
    # Use `transcript` instead of `tx` consistently.
    colnames(mcols) <- gsub(
        pattern = "^tx",
        replacement = "transcript",
        x = colnames(mcols)
    )
    # Remove columns that are all NA.
    mcols <- removeNA(mcols)

    # Missing `geneName`
    if (!"geneName" %in% colnames(mcols)) {
        # nocov start
        warning("`geneName` is missing. Using `geneID` instead.")
        assert_is_subset("geneID", colnames(mcols))
        mcols[["geneName"]] <- mcols[["geneID"]]
        # nocov end
    }

    # Missing `transcriptName`.
    if (
        "transcriptID" %in% colnames(mcols) &&
        !"transcriptName" %in% colnames(mcols)
    ) {
        # nocov start
        warning("`transcriptName` is missing. Using `transcriptID` instead.")
        mcols[["transcriptName"]] <- mcols[["transcriptID"]]
        # nocov end
    }

    # Always use `geneName` instead of `symbol`.
    if (all(c("geneName", "symbol") %in% colnames(mcols))) {
        mcols[["symbol"]] <- NULL
    } else if ("symbol" %in% colnames(mcols)) {
        # nocov start
        mcols[["geneName"]] <- mcols[["symbol"]]
        mcols[["symbol"]] <- NULL
        # nocov end
    }

    # Sanitize any character columns that have duplicates into factor.
    mcols <- lapply(
        X = mcols,
        FUN = function(col) {
            if (is.character(col) && any(duplicated(col))) {
                as.factor(col)
            } else {
                # `I` inhibits reinterpretation and returns AsIs.
                # Recommended in the DataFrame documentation.
                I(col)
            }
        }
    )
    mcols <- as(mcols, "DataFrame")
    mcols(object) <- mcols

    # Require that names match the identifier column.
    # Use `transcriptID` over `geneID` if defined.
    assert_are_intersecting_sets(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(object))
    )
    if ("transcriptID" %in% colnames(mcols(object))) {
        idCol <- "transcriptID"
    } else {
        idCol <- "geneID"
    }
    names(object) <- mcols(object)[[idCol]]

    # Ensure broad class definitions are included.
    mcols(object)[["broadClass"]] <- broadClass(object)

    # Sort metadata columns alphabetically
    mcols(object) <- mcols(object)[, sort(colnames(mcols(object)))]

    # Ensure GRanges is sorted by names
    message(paste0("Sorting ranges by ", idCol, "..."))
    object <- object[sort(names(object))]

    assert_is_all_of(object, "GRanges")
    object
}



# Report the source of the gene annotations.
.gffSource <- function(gff) {
    assert_is_subset("source", colnames(mcols(gff)))
    if (any(grepl("FlyBase", mcols(gff)[["source"]]))) {
        "FlyBase"  # nocov
    } else if (any(grepl("WormBase", mcols(gff)[["source"]]))) {
        "WormBase"  # nocov
    } else if (any(grepl(
        "ensembl", mcols(gff)[["source"]], ignore.case = TRUE
    ))) {
        "Ensembl"
    } else {
        stop("Unsupported GFF source.")  # nocov
    }
}



# Determine if GFF or GTF.
.gffType <- function(gff) {
    stopifnot(is(gff, "GRanges"))
    gff <- camel(gff)
    if (all(c("id", "name") %in% colnames(mcols(gff)))) {
        "GFF"
    } else {
        "GTF"
    }
}



# Aliases ======================================================================
#' @rdname makeGRanges
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF



# Legacy =======================================================================
#' @rdname makeGRanges
#' @export
annotable <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall()
        )
        as(gr, "tbl_df")
    }
formals(annotable) <- formals(makeGRangesFromEnsembl)
