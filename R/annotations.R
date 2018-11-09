# TODO Can we suppress this warning consistently?
# `Warning: call dbDisconnect() when finished working with a connection`
# Still an issue with Bioconductor infrastructure.



# convertUCSCBuildToEnsembl ====================================================
#' Convert UCSC Build to Ensembl
#'
#' @export
#'
#' @inheritParams params
#'
#' @return `character`. Ensembl genome build as the value, UCSC build as the
#'   name. Stops on match failure.
#'
#' @examples
#' from <- c("hg19", "hg38")
#' to <- convertUCSCBuildToEnsembl(from)
#' print(to)
convertUCSCBuildToEnsembl <- function(object) {
    assert_is_character(object)
    keys <- c(
        # Homo sapiens (Human)
        "GRCh37" = "hg19",
        "GRCh38" = "hg38",
        # Mus musculus (Mouse)
        "GRCm38" = "mm10",
        # Rattus norvegicus (Rat)
        "Rnor_6.0" = "rn6",
        # Drosophila melanogaster (Fruitfly)
        "BDGP6" = "dm6",
        # Caenorhabditis elegans (Worm)
        "WBcel235" = "ce11",
        # Saccharomyces cerevisiae (Yeast)
        "R64-1-1" = "sacCer3"
    )
    match <- match(x = object, table = keys)
    # Stop on match failure.
    if (any(is.na(match))) {
        stop(paste(
            "Failed to match UCSC to Ensembl:",
            toString(object[which(is.na(match))])
        ))
    }
    return <- names(keys)[match]
    names(return) <- keys[match]
    return
}



# emptyRanges ==================================================================
#' Generate Empty Genomic Ranges
#'
#' Utility function that provides support for creating internal `GRanges` for
#' transgene and FASTA spike-in sequences.
#'
#' @export
#'
#' @param names `character`. Gene or transcript names.
#' @param seqname `string` Name of the alternative chromosome to be defined in
#'   [GenomeInfoDb::seqnames()] where these ranges will be grouped. Defaults to
#'   "`unknown`" but "`transgene`" (transgenes) and "`spike`" (spike-ins) are
#'   also supported.
#' @param mcolsNames `character` or `NULL`. Metadata column names to be defined
#'   in the [S4Vectors::mcols()] of the `GRanges` return. Normally this does not
#'   need to be defined; useful when combining with another `GRanges` that
#'   contains metadata.
#'
#' @return `GRanges`.
#'
#' @seealso `help("seqinfo", "GenomeInfoDb")`.
#'
#' @examples
#' ## Unknown/dead genes.
#' emptyRanges("ENSG00000000000", seqname = "unknown")
#'
#' ## Transgenes.
#' emptyRanges(c("EGFP", "TDTOMATO", "GAL4"), seqname = "transgene")
#'
#' ## Spike-ins.
#' emptyRanges("ERCC", seqname = "spike")
emptyRanges <- function(
    names,
    seqname = c("unknown", "transgene", "spike"),
    mcolsNames = NULL
) {
    assert_is_character(names)
    assert_all_are_non_missing_nor_empty_character(names)
    seqname <- match.arg(seqname)
    assert_is_any_of(mcolsNames, c("character", "NULL"))

    gr <- GRanges(
        seqnames = seqname,
        ranges = IRanges(
            start = (seq_len(length(names)) - 1L) * 100L + 1L,
            width = 100L
        )
    )
    names(gr) <- names

    # Create the required empty metadata columns.
    if (!has_length(mcolsNames)) {
        ncol <- 0L
    } else {
        ncol <- length(mcolsNames)
    }
    mcols <- matrix(
        nrow = length(names),
        ncol = ncol,
        dimnames = list(
            names,
            mcolsNames
        )
    )
    mcols <- as(mcols, "DataFrame")
    mcols(gr) <- mcols

    gr
}



# geneSynonyms =================================================================
#' Gene Synonyms
#'
#' Look up gene synonyms from NCBI.
#'
#' @note Synonym support for *Caenorhabditis elegans* is poor on NCBI.
#' Use the [wormbase](https://steinbaugh.com/wormbase) package instead.
#'
#' @export
#'
#' @inheritParams params
#' @param organism `string`. Supported organisms: *Homo sapiens*,
#'   *Mus musculus*, *Drosophila melanogaster*.
#'
#' @return `grouped_df`. Grouped by `geneID` column.
#'
#' @examples
#' options(basejump.test = TRUE)
#' x <- geneSynonyms(organism = "Homo sapiens")
#' print(x)
geneSynonyms <- function(organism) {
    assert_that(has_internet())
    organism <- match.arg(
        arg = organism,
        choices = .geneSynonymsOrganisms
    )

    # NCBI uses underscore for species name
    species <- gsub(" ", "_", organism)
    if (species == "Drosophila_melanogaster") {
        kingdom <- "Invertebrates"
    } else {
        kingdom <- "Mammalia"
    }

    genome <- c(kingdom = kingdom, species = species)

    if (isTRUE(getOption("basejump.test"))) {
        assert_that(organism == "Homo sapiens")
        file <- file.path(
            basejumpCacheURL,
            paste0(snake(organism), ".gene_info.gz")
        )
    } else {
        file <- paste(
            "ftp://ftp.ncbi.nih.gov",
            "gene",
            "DATA",
            "GENE_INFO",
            genome[["kingdom"]],
            paste0(genome[["species"]], ".gene_info.gz"),
            sep = "/"
        )
    }

    data <- read_tsv(
        file = file,
        col_types = cols(),
        progress = FALSE
    )
    assert_is_non_empty(data)

    data <- data %>%
        camel() %>%
        select(!!!syms(c("symbol", "synonyms", "dbXrefs"))) %>%
        rename(geneName = !!sym("symbol")) %>%
        filter(
            !!sym("synonyms") != "-",
            !!sym("dbXrefs") != "-"
        ) %>%
        mutate(synonyms = str_replace_all(!!sym("synonyms"), "\\|", ", "))

    # Sanitize the identifiers.
    if (organism == "Drosophila melanogaster") {
        data <- mutate(
            data,
            geneID = str_extract(!!sym("dbXrefs"), "\\bFBgn[0-9]{7}\\b")
        )
    } else {
        data <- mutate(
            data,
            geneID = str_extract(!!sym("dbXrefs"), "\\bENS[A-Z]+[0-9]{11}\\b")
        )
    }

    data %>%
        filter(!is.na(!!sym("geneID"))) %>%
        select(!!!syms(c("geneID", "geneName", "synonyms"))) %>%
        arrange(!!sym("geneID")) %>%
        group_by(!!sym("geneID"))
}



# Using this for parameterized unit testing.
.geneSynonymsOrganisms <- c(
    "Homo sapiens",
    "Mus musculus",
    "Drosophila melanogaster"
)



# makeGRanges ==================================================================
#' Make Genomic Ranges
#'
#' @name makeGRanges
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
#' @section GRCh37 (hg19) legacy annotations:
#'
#' [makeGRangesFromEnsembl()] supports the legacy *Homo sapiens* GRCh37 (release
#' 75) build by internally querying the [EnsDb.Hsapiens.v75][] package.
#' Alternatively, the corresponding GTF/GFF file can be loaded directly from
#' GENCODE or Ensembl.
#'
#' [EnsDb.Hsapiens.v75]: https://doi.org/doi:10.18129/B9.bioc.EnsDb.Hsapiens.v75
#'
#' @inheritParams params
#' @param genomeBuild `string` or `NULL`. Ensembl genome build assembly name
#'   (e.g. `"GRCh38"`). If set `NULL`, defaults to the most recent build
#'   available. Note: don't pass in UCSC build IDs (e.g. `"hg38"`).
#' @param release `scalar integer` or `NULL`. Ensembl release version (e.g.
#'   `90`). If set `NULL`, defaults to the most recent release available.
#'
#' @return `GRanges`.
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#' - [rtracklayer::import()].
#' - [GenomicFeatures::makeTxDbFromGFF()].
#'
#' @examples
#' ## makeGRangesFromEnsembl ====
#' ## Genes
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "transcripts")
#' summary(x)
#'
#' ## makeGRangesFromEnsDb ====
#' x <- makeGRangesFromEnsDb("EnsDb.Hsapiens.v75")
#'
#' ## makeGRangesFromGFF ====
#' file <- file.path(basejumpCacheURL, "example.gtf")
#'
#' ## Genes
#' x <- makeGRangesFromGFF(file = file, level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromGFF(file = file, level = "transcripts")
#' summary(x)
NULL



#' Connect to AnnotationHub
#'
#' On a fresh install this will print a txProgressBar to the console. We're
#' using [utils::capture.output()] here to suppress the console output, since
#' it's not very informative and can cluster R Markdown reports.
#'
#' @noRd
.annotationHub <- function() {
    userAttached <- .packages()
    invisible(capture.output(
        ah <- suppressMessages(AnnotationHub())
    ))
    assert_is_all_of(ah, "AnnotationHub")
    .forceDetach(keep = userAttached)
    ah
}



#' Broad Class Definitions
#'
#' @author Rory Kirchner, Michael Steinbaugh
#' @noRd
#'
#' @inheritParams params
#' @param object Object that can be coerced to `DataFrame`, containing gene or
#'   transcript annotations. `GRanges` is recommended.
#'
#' @return Named `factor` containing broad class definitions.
.broadClass <- function(object) {
    assert_is_all_of(object, "GRanges")

    names <- names(object)
    assert_is_character(names)
    assert_is_non_empty(names)

    data <- as_tibble(object)

    # Early return if already defined.
    if ("broadClass" %in% colnames(data)) {
        broad <- data[["broadClass"]]
        names(broad) <- names
        return(broad)
    }

    # Gene name (required).
    assert_is_subset("geneName", colnames(data))
    geneName <- data[["geneName"]]

    # Biotype (optional).
    # Prioritize transcript over gene, if present.
    biotypeCol <- grep(
        pattern = "biotype$",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    if (has_length(biotypeCol)) {
        biotypeCol <- biotypeCol[[1L]]
        biotype <- data[[biotypeCol]]
    } else {
        # nocov start
        warning("Biotype column is missing.", call. = FALSE)
        biotype <- NA
        # nocov end
    }

    # Seqname (optional; aka chromosome).
    seqnameCol <- grep(
        pattern = "seqname",
        x = colnames(data),
        ignore.case = TRUE,
        value = TRUE
    )
    if (has_length(seqnameCol)) {
        seqnameCol <- seqnameCol[[1L]]
        seqname <- data[[seqnameCol]]
    } else {
        # nocov start
        warning("`seqname` column is missing.", call. = FALSE)
        seqname <- NA
        # nocov end
    }

    message(paste(
        "Defining broadClass using:",
        toString(c("geneName", biotypeCol, seqnameCol))
    ))

    data <- tibble(
        geneName = geneName,
        biotype = biotype,
        seqname = seqname
    )

    broad <- case_when(
        data[["seqname"]] == "MT" ~ "mito",
        grepl(
            pattern = "^mt[\\:\\-]",
            x = data[["geneName"]],
            ignore.case = TRUE
        ) ~ "mito",
        data[["biotype"]] == "protein_coding" ~ "coding",
        data[["biotype"]] %in% c(
            "known_ncrna",
            "lincRNA",
            "non_coding"
        ) ~ "noncoding",
        grepl(
            pattern = "pseudo",
            x = data[["biotype"]]
        ) ~ "pseudo",
        data[["biotype"]] %in% c(
            "miRNA",
            "misc_RNA",
            "ribozyme",
            "rRNA",
            "scaRNA",
            "scRNA",
            "snoRNA",
            "snRNA",
            "sRNA"
        ) ~ "small",
        data[["biotype"]] %in% c(
            "non_stop_decay",
            "nonsense_mediated_decay"
        ) ~ "decaying",
        grepl(
            pattern = "^ig_",
            x = data[["biotype"]],
            ignore.case = TRUE
        ) ~ "ig",
        grepl(
            pattern = "^tr_",
            x = data[["biotype"]],
            ignore.case = TRUE
        ) ~ "tcr",
        # Consider using `NA_character_` here instead.
        TRUE ~ "other"
    )

    broad <- as.factor(broad)
    names(broad) <- names
    broad
}



#' Force Detach Packages
#'
#' ensembldb will attach unwanted packages into the NAMESPACE, which can
#' conflict with tidyverse packages (e.g. dplyr).
#'
#' @noRd
.forceDetach <- function(keep = NULL) {
    detach <- setdiff(.packages(), keep)
    if (has_length(detach)) {
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
    assert_are_identical(.packages(), keep)
}



#' Get AnnotationHub ID
#' @noRd
#' @examples .getAnnotationHubID("Homo sapiens")
.getAnnotationHubID <- function(
    organism,
    genomeBuild = NULL,
    ensemblRelease = NULL,
    ah = NULL
) {
    userAttached <- .packages()
    assert_is_a_string(organism)
    # Standardize organism name, if necessary.
    organism <- gsub("_", " ", makeNames(organism))
    assertIsAStringOrNULL(genomeBuild)
    # Check for accidental UCSC input and stop, informing user.
    if (is_a_string(genomeBuild)) {
        ucscCheck <- tryCatch(
            expr = convertUCSCBuildToEnsembl(genomeBuild),
            error = function(e) NULL
        )
        if (has_length(ucscCheck)) {
            stop(paste(
                "UCSC genome build ID detected.",
                "Use Ensembl ID instead.\n",
                printString(ucscCheck)
            ))
        }
    }
    assertIsAnImplicitIntegerOrNULL(ensemblRelease)
    if (isAnImplicitInteger(ensemblRelease)) {
        # Note that ensembldb currently only supports >= 87.
        assert_all_are_positive(ensemblRelease)
        ensemblRelease <- as.integer(ensemblRelease)
    }

    # Error on request of unsupported legacy Ensembl release.
    if (!is.null(ensemblRelease) && ensemblRelease < 87L) {
        stop(paste(
            "ensembldb currently only supports Ensembl releases >= 87."
        ))
    }

    # Get AnnotationHub if necessary.
    if (is.null(ah)) {
        ah <- .annotationHub()
    }

    # Matching EnsDb objects from ensembldb by default.
    rdataclass <- "EnsDb"

    message(paste0(
        "Matching ", rdataclass, " from AnnotationHub ",
        packageVersion("AnnotationHub"),
        " (", snapshotDate(ah), ")."
    ))

    # Query AnnotationHub.
    ahs <- query(
        x = ah,
        pattern = c(
            "Ensembl",
            organism,
            genomeBuild,
            ensemblRelease,
            rdataclass
        ),
        ignore.case = TRUE
    )

    # Get the AnnotationHub from the metadata columns.
    mcols <- mcols(ahs)

    # Abort if there's no match and working offline.
    if (!isTRUE(has_internet()) && !nrow(mcols)) {
        # nocov start
        stop(
            "AnnotationHub requires an Internet connection for query.",
            call. = FALSE
        )
        # nocov end
    }

    # Ensure genome build matches, if specified.
    if (!is.null(genomeBuild)) {
        assert_is_subset("genome", colnames(mcols))
        mcols <- mcols[mcols[["genome"]] %in% genomeBuild, , drop = FALSE]
    }

    # Ensure Ensembl release matches, or pick the latest one.
    if (!is.null(ensemblRelease)) {
        assert_is_subset("title", colnames(mcols))
        mcols <- mcols[
            grepl(paste("Ensembl", ensemblRelease), mcols[["title"]]),
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
            paste(.li, "Organism:", deparse(organism)),
            paste(.li, "Build:", deparse(genomeBuild)),
            paste(.li, "Release:", deparse(ensemblRelease)),
            sep = "\n"
        ))
    }

    mcols <- tail(mcols, n = 1L)
    id <- rownames(mcols)
    assert_is_a_string(id)
    assert_all_are_matching_regex(x = id, pattern = "^AH[[:digit:]]+$")
    message(paste0(id, ": ", mcols[["title"]]))
    .forceDetach(keep = userAttached)
    id
}



#' Get EnsDb from AnnotationHub
#' @noRd
#' @examples .getEnsDbFromAnnotationHub("AH64923")
.getEnsDbFromAnnotationHub <- function(id, ah = NULL) {
    userAttached <- .packages()
    # Get AnnotationHub if necessary.
    if (is.null(ah)) {
        ah <- .annotationHub()
    }
    assert_is_all_of(ah, "AnnotationHub")
    # This step will also output `txProgressBar()` on a fresh install. Using
    # `capture.output()` here again to suppress console output.
    # Additionally, it attaches ensembldb and other Bioconductor dependency
    # packages, which will mask some tidyverse functions (e.g. `select()`).
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))
    assert_is_all_of(edb, "EnsDb")
    .forceDetach(keep = userAttached)
    edb
}



#' Get EnsDb from Package
#' @noRd
#' @examples .getEnsDbFromPackage("EnsDb.Hsapiens.v75")
.getEnsDbFromPackage <- function(package) {
    message(paste0("Getting EnsDb from ", package, "."))
    userAttached <- .packages()
    assert_is_a_string(package)
    require(package, character.only = TRUE)
    edb <- get(
        x = package,
        envir = asNamespace(package),
        inherits = FALSE
    )
    assert_is_all_of(edb, "EnsDb")
    .forceDetach(keep = userAttached)
    edb
}



# Report the source of the gene annotations.
.gffSource <- function(gff) {
    assert_is_subset("source", colnames(mcols(gff)))
    if (
        any(grepl("FlyBase", mcols(gff)[["source"]]))
    ) {
        "FlyBase"  # nocov
    } else if (
        any(grepl("WormBase", mcols(gff)[["source"]]))
    ) {
        "WormBase"  # nocov
    } else if (
        any(grepl("Ensembl", mcols(gff)[["source"]], ignore.case = TRUE))
    ) {
        "Ensembl"
    } else {
        stop("Unsupported GFF source.")  # nocov
    }
}



# Determine if GFF or GTF.
.gffType <- function(gff) {
    assert_that(is(gff, "GRanges"))
    gff <- camel(gff)
    if (all(c("id", "name") %in% colnames(mcols(gff)))) {
        "GFF"
    } else {
        "GTF"
    }
}



.makeGRanges <- function(object) {
    assert_is_all_of(object, "GRanges")
    assert_has_names(object)

    # Standardize the metadata columns.
    mcols <- mcols(object)
    # Sanitize to camel case.
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

    # Missing `geneName`.
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
    mcols(object)[["broadClass"]] <- .broadClass(object)

    # Sort metadata columns alphabetically.
    mcols(object) <- mcols(object)[, sort(colnames(mcols(object)))]

    # Ensure GRanges is sorted by names.
    message(paste0("Arranging by ", idCol, "."))
    object <- object[sort(names(object))]

    assert_is_all_of(object, "GRanges")
    object
}



#' @describeIn makeGRanges
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
#' @export
makeGRangesFromEnsembl <- function(
    organism,
    level = c("genes", "transcripts"),
    genomeBuild = NULL,
    release = NULL
) {
    message("Making GRanges from Ensembl.")
    assert_is_a_string(organism)
    level <- match.arg(level)
    if (
        identical(tolower(organism), "homo sapiens") &&
        (
            identical(tolower(as.character(genomeBuild)), "grch37") ||
            identical(release, 75L)
        )
    ) {
        id <- "EnsDb.Hsapiens.v75"
        edb <- .getEnsDbFromPackage(package = id)
    } else {
        id <- .getAnnotationHubID(
            organism = organism,
            genomeBuild = genomeBuild,
            ensemblRelease = release
        )
        edb <- .getEnsDbFromAnnotationHub(id = id)
    }
    gr <- makeGRangesFromEnsDb(object = edb, level = level)
    metadata(gr)[["id"]] <- id
    gr
}



#' @describeIn makeGRanges Use specific `EnsDb` object as annotation source.
#'   Alternatively, can pass in an EnsDb package name as a `string`.
#' @export
makeGRangesFromEnsDb <- function(object, level) {
    message("Making GRanges from EnsDb object.")
    userAttached <- .packages()

    # Allow loading of EnsDb package, passed in as a character string.
    if (is_a_string(object)) {
        package <- object
        requireNamespace(package = package)
        object <- get(
            x = package,
            envir = asNamespace(package),
            inherits = FALSE
        )
    }
    assert_that(is(object, "EnsDb"))

    assert_is_all_of(object, "EnsDb")
    level <- match.arg(level)

    # Get the genome build from the ensembldb metdata.
    genomeBuild <- metadata(object) %>%
        as_tibble() %>%
        filter(!!sym("name") == "genome_build") %>%
        pull("value")
    assert_is_a_string(genomeBuild)

    # Define the metadata to return.
    metadata <- c(
        .prototypeMetadata,
        list(
            organism = organism(object),
            genomeBuild = genomeBuild,
            ensemblRelease = as.integer(ensemblVersion(object)),
            ensembldb = metadata(object),
            level = level
        )
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
        assert_are_identical(
            x = txData[["tx_id"]],
            y = data[["tx_id"]]
        )

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



#' @describeIn makeGRanges
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2. We recommend
#' using a GTF file instead of a GFF3 file, if possible.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' Remote URLs and compressed files are supported.
#' @export
makeGRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts")
) {
    message("Making GRanges from GFF/GTF file.")
    # Note that `import()` has assert checks for file (see below).
    level <- match.arg(level)

    # Import -------------------------------------------------------------------
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
        # Remove extra columns.
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
            assert_that(all(grepl("^gene:", mcols(tx)[["parent"]])))
            mcols(tx)[["geneID"]] <- as.character(mcols(tx)[["parent"]])
            mcols(tx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["geneID"]]
            )
            # Remove extra columns.
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

    # Metadata -----------------------------------------------------------------
    organism <- tryCatch(
        expr = organism(gr),
        error = function(e) character()
    )
    metadata(gr) <- c(
        .prototypeMetadata,
        list(
            file = file,
            level = level,
            organism = organism,
            genomeBuild = character(),
            ensemblRelease = integer()
        )
    )

    .makeGRanges(gr)
}



#' @rdname makeGRanges
#' @usage NULL
#' @export
makeGRangesFromGTF <- makeGRangesFromGFF



#' @describeIn makeGRanges
#' [annotable()] is a legacy convenience function that calls
#' [makeGRangesFromEnsembl()] and returns a `tibble` instead of `GRanges`. Note
#' that `GRanges` can also be coercing using [BiocGenerics::as.data.frame()].
#' @export
annotable <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall()
        )
        as_tibble(gr, rownames = NULL)
    }
formals(annotable) <- formals(makeGRangesFromEnsembl)



# makeGene2Symbol ==============================================================
#' Make Gene-to-Symbol Mappings
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeGene2Symbol
#' @inheritParams makeGRanges
#'
#' @seealso [makeGRanges].
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' ## makeGene2SymbolFromEnsembl ====
#' x <- makeGene2SymbolFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' ## makeTx2GeneFromEnsDb ====
#' x <- makeGene2SymbolFromEnsDb("EnsDb.Hsapiens.v75")
#' print(x)
#'
#' ## makeGene2SymbolFromGFF ====
#' ## GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeGene2SymbolFromGFF(file)
#' print(x)
#'
#' ## GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeGene2SymbolFromGFF(file)
#' print(x)
NULL



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "genes"))
        )
        Gene2Symbol(gr)
    }
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), "level")]
formals(makeGene2SymbolFromEnsembl) <- f



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromEnsDb <- function(object) {
    gr <- makeGRangesFromEnsDb(object)
    Gene2Symbol(gr)
}



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromGFF <- function(file) {
    gr <- makeGRangesFromGFF(file, level = "genes")
    Gene2Symbol(gr)
}



#' @rdname makeGene2Symbol
#' @usage NULL
#' @export
makeGene2SymbolFromGTF <- makeGene2SymbolFromGFF



# makeTx2Gene ==================================================================
#' Make Transcript-to-Gene Mappings
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeTx2Gene
#' @inheritParams makeGRanges
#'
#' @seealso [makeGRanges].
#'
#' @return `Tx2Gene`.
#'
#' @examples
#' ## makeTx2GeneFromEnsembl ====
#' x <- makeTx2GeneFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' ## makeTx2GeneFromEnsDb ====
#' x <- makeTx2GeneFromEnsDb("EnsDb.Hsapiens.v75")
#' print(x)
#'
#' ## makeTx2GeneFromGFF ====
#' ## GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeTx2GeneFromGFF(file)
#' print(x)
#'
#' ## GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeTx2GeneFromGFF(file)
#' print(x)
NULL



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "transcripts"))
        )
        Tx2Gene(gr)
    }
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), c("level", "metadata", "..."))]
formals(makeTx2GeneFromEnsembl) <- f



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromEnsDb <- function(object) {
    gr <- makeGRangesFromEnsDb(object, level = "transcripts")
    Tx2Gene(gr)
}



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromGFF <- function(file) {
    gr <- makeGRangesFromGFF(file, level = "transcripts")
    Tx2Gene(gr)
}



#' @rdname makeTx2Gene
#' @usage NULL
#' @export
makeTx2GeneFromGTF <- makeTx2GeneFromGFF
