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
#'   - `coding`
#'   - `noncoding`
#'   - `pseudo`
#'   - `small`
#'   - `decaying`
#'   - `ig` (immunoglobulin)
#'   - `tcr` (T cell receptor)
#'   - `other`
#'
#' @section annotable:
#'
#' [annotable()] is a legacy convenience function that calls
#' [makeGRangesFromEnsembl()] and returns a `data.frame` instead of `GRanges`.
#' Note that `GRanges` also support coercion to a basic `data.frame` using
#' [as.data.frame()].
#'
#' @inheritParams general
#' @param level `string`. Return ranges as "`genes`" or "`transcripts`".
#' @param build `string` or `NULL`. Genome build assembly name (e.g.
#'   "`GRCh38`"). If set `NULL`, defaults to the most recent build available.
#' @param release `scalar integer` or `NULL`. Release version (e.g. `90`). If
#'   set `NULL`, defaults to the most recent release available.
#' @param metadata `boolean`. Include the AnnotationHub metadata inside a
#'   `list`.
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
#' file <- "http://basejump.seq.cloud/example.gtf"
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
    release = NULL,
    metadata = FALSE,
    ...
) {
    # Legacy arguments ---------------------------------------------------------
    call <- match.call()
    # format
    if ("format" %in% names(call)) {
        warning("Use `level` instead of `format`")
        level <- call[["format"]]
    }

    # Assert checks ------------------------------------------------------------
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
        if (length(ucscCheck)) {
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
    assert_is_a_bool(metadata)

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
        id <- "EnsDb.Hsapiens.v75"
        if (requireNamespace(id, quietly = TRUE)) {
            edb <- get(id, envir = asNamespace(id), inherits = FALSE)
        } else {
            # nocov start
            stop(paste(
                "GRCh37 genome build requires the", id, "package"
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

        message(paste(
            "Making GRanges from Ensembl with AnnotationHub",
            packageVersion("AnnotationHub"),
            paste0("(", snapshotDate(ah), ")")
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
            stop("AnnotationHub requires an Internet connection for lookup")
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
                paste(
                    "No ID matched on AnnotationHub",
                    packageVersion("AnnotationHub")
                ),
                paste("organism:", deparse(organism)),
                paste("build:", deparse(build)),
                paste("release:", deparse(release)),
                sep = "\n"
            ))
        }

        mcols <- tail(mcols, 1L)
        message(mcols[["title"]])
        id <- rownames(mcols)

        # This step will also output `txProgressBar()` on a fresh install. Using
        # `capture.output()` here again to suppress console output.
        # Additionally, it attaches ensembldb and other Bioconductor dependency
        # packages, which will mask some tidyverse functions (e.g. `select()`).
        invisible(capture.output(
            edb <- suppressMessages(ah[[id]])
        ))
    }

    # Get annotations from EnsDb -----------------------------------------------
    assert_is_all_of(edb, "EnsDb")

    meta <- metadata(edb)
    assert_is_data.frame(meta)
    meta <- rbind(c("id", id), meta)
    meta <- as(meta, "tbl_df")

    # Stash the AnnotationHub ID.
    build <- meta[meta[["name"]] == "genome_build", "value", drop = TRUE]
    assert_is_a_string(build)

    message(paste(
        paste("id:", deparse(id)),
        paste("organism:", deparse(organism(edb))),
        paste("build:", deparse(build)),
        paste("release:", deparse(ensemblVersion(edb))),
        paste("level:", deparse(level)),
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

    gr <- .makeGRanges(gr)

    # Include the EnsDB metadata inside a list, if desired.
    if (isTRUE(metadata)) {
        list(data = gr, metadata = meta)
    } else {
        gr
    }
}



#' @rdname makeGRanges
#' @export
makeGRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts"),
    ...
) {
    # Legacy arguments ---------------------------------------------------------
    call <- match.call()
    # format
    if ("format" %in% names(call)) {
        warning("Use `level` instead of `format`")
        level <- call[["format"]]
    }

    # Assert checks ------------------------------------------------------------
    # Note that `readGFF()` has assert checks for file (see below).
    level <- match.arg(level)

    # Read GFF -----------------------------------------------------------------
    file <- localOrRemoteFile(file)
    gff <- readGFF(file)
    assert_is_all_of(gff, "GRanges")
    gff <- camel(gff)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected"))

    # nocov start
    if (
        source %in% c("FlyBase", "WormBase") &&
        type == "GFF"
    ) {
        stop(paste(
            "Only GTF files are currently supported from", source
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
        message(paste(length(gn), "gene annotations"))
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

        message(paste(length(tx), "transcript annotations"))
        gr <- tx

        # Merge the gene-level annotations (`geneName`, `geneBiotype`).
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(gr))
        )
        if (length(geneCols)) {
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

    .makeGRanges(gr)
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
            args = as.list(match.call())[-1L]
        )
        as(gr, "tbl_df")
    }
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), "metadata")]
formals(annotable) <- f
