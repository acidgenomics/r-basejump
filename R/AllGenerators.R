## FIXME Switch to base R approaches here.



#' All generator functions
#' @include AllGenerics.R
#' @noRd
NULL



## EggNOG ======================================================================
#' @inherit EggNOG-class title description return
#' @note Updated 2019-08-15.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- EggNOG()
#' print(x)
EggNOG <-  # nolint
    function() {
        assert(hasInternet())
        if (isTRUE(getOption("acid.test"))) {
            categoriesFile <- pasteURL(
                basejumpTestsURL, "cog.txt",
                protocol = "none"
            )
            eunogFile <- pasteURL(
                basejumpTestsURL, "eunog.tsv.gz",
                protocol = "none"
            )
            nogFile <- pasteURL(
                basejumpTestsURL, "nog.tsv.gz",
                protocol = "none"
            )
        } else {
            ## This is slow and unreliable on Travis, so cover locally.
            ## EggNOG database doesn't support HTTPS currently.
            ## nocov start
            url <- pasteURL(
                "eggnog5.embl.de", "download", "latest",
                protocol = "http"
            )
            categoriesFile <- pasteURL(
                url, "COG_functional_categories.txt",
                protocol = "none"
            )
            eunogFile <- pasteURL(
                url, "data", "euNOG", "euNOG.annotations.tsv.gz",
                protocol = "none"
            )
            nogFile <- pasteURL(
                url, "data", "NOG", "NOG.annotations.tsv.gz",
                protocol = "none"
            )
            ## nocov end
        }
        assert(
            isString(categoriesFile),
            isString(eunogFile),
            isString(nogFile)
        )

        ## Categories ----------------------------------------------------------
        pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
        x <- readLines(categoriesFile)
        x <- str_subset(x, pattern)
        x <- str_match(x, pattern)
        x <- as(x, "DataFrame")
        x <- x[, c(2L, 3L)]
        colnames(x) <- c("letter", "description")
        x <- x[order(x[["letter"]]), , drop = FALSE]
        categories <- x

        ## Annotations ---------------------------------------------------------
        colnames <- c(
            "taxonomicLevel",
            "groupName",
            "proteinCount",
            "speciesCount",
            "cogFunctionalCategory",
            "consensusFunctionalDescription"
        )
        ## euNOG: Eukaryota
        eunog <- as(import(eunogFile, colnames = FALSE), "DataFrame")
        colnames(eunog) <- colnames
        ## NOG: LUCA
        nog <- as(import(nogFile, colnames = FALSE), "DataFrame")
        ## Bind annotations.
        colnames(nog) <- colnames
        x <- rbind(eunog, nog)
        x <- x[
            ,
            c(
                "groupName",
                "consensusFunctionalDescription",
                "cogFunctionalCategory"
            )
            ]
        colnames(x)[colnames(x) == "groupName"] <- "eggnogID"
        x <- x[order(x[["eggnogID"]]), , drop = FALSE]
        annotations <- x

        ## Return --------------------------------------------------------------
        data <- List(
            cogFunctionalCategories = categories,
            annotations = annotations
        )
        metadata(data) <- .prototypeMetadata
        new(Class = "EggNOG", data)
    }



## Ensembl2Entrez ==============================================================
#' @inherit Ensembl2Entrez-class title description return
#' @name Ensembl2Entrez
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param format `character(1)`.
#'   Formatting method to apply:
#'
#'   - `"1:1"`: *Recommended.*
#'       Return with 1:1 mappings. For Ensembl genes that don't map 1:1 with
#'       Entrez, pick the oldest Entrez ID. Genes that don't map to Entrez will
#'       contain `NA` in `entrezID` column.
#'   - `"long"`:
#'       Return 1:many in long format.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



## Updated 2019-07-22.
`Ensembl2Entrez,DataFrame` <-  # nolint
    function(object, format = c("1:1", "long")) {
        assert(hasRows(object))
        format <- match.arg(format)

        cols <- c("geneID", "entrezID")
        if (!all(cols %in% colnames(object))) {
            stop(sprintf(
                "Object does not contain Ensembl-to-Entrez mappings: %s.",
                toString(cols)
            ))
        }

        data <- DataFrame(
            geneID = as.character(decode(object[["geneID"]])),
            entrezID = I(object[["entrezID"]]),
            row.names = rownames(object)
        )

        ## Expand to long format.
        data <- expand(data)

        ## Inform the user about genes that don't map to Entrez.
        unmapped <- data[["geneID"]][which(is.na(data[["entrezID"]]))]
        assert(hasNoDuplicates(unmapped))
        if (length(unmapped) > 0L) {
            message(sprintf("%d genes don't map to Entrez.", length(unmapped)))
        }

        ## Inform the user about how many genes multi-map to Entrez.
        multimapped <- unique(data[["geneID"]][duplicated(data[["geneID"]])])
        if (length(multimapped) > 0L) {
            message(sprintf(
                "%d genes map to multiple Entrez IDs.", length(multimapped)
            ))
        }

        if (format == "1:1") {
            message(
                "Returning with 1:1 mappings using oldest Entrez ID per gene."
            )
            entrez <- object[["entrezID"]]
            assert(is.list(entrez))
            names(entrez) <- object[["geneID"]]
            map <- lapply(
                X = entrez,
                FUN = function(x) {
                    if (all(is.na(x))) {
                        NA_integer_
                    } else {
                        sort(x)[[1L]]
                    }
                }
            )
            entrez <- unlist(map)
            data <- DataFrame(
                geneID = names(entrez),
                entrezID = as.integer(entrez),
                row.names = rownames(object)
            )
        } else if (format == "long") {
            message("Returning 1:many in long format (not recommended).")
        }

        metadata(data) <- metadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Ensembl2Entrez", data)
    }



## Updated 2019-07-22.
`Ensembl2Entrez,GRanges` <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        metadata(data) <- metadata(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = data,
                format = format
            )
        )
    }

formals(`Ensembl2Entrez,GRanges`) <- formals(`Ensembl2Entrez,DataFrame`)



## Updated 2019-07-22.
`Ensembl2Entrez,SummarizedExperiment` <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = data,
                format = format
            )
        )


    }

formals(`Ensembl2Entrez,SummarizedExperiment`) <-
    formals(`Ensembl2Entrez,DataFrame`)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("DataFrame"),
    definition = `Ensembl2Entrez,DataFrame`
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("GRanges"),
    definition = `Ensembl2Entrez,GRanges`
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("SummarizedExperiment"),
    definition = `Ensembl2Entrez,SummarizedExperiment`
)



## Gene2Symbol =================================================================
#' @inherit Gene2Symbol-class title description return
#' @name Gene2Symbol
#'
#' @note For some organisms, gene names and gene symbols do not map 1:1 (e.g.
#'   *Homo sapiens* and *Mus musculus*). Refer to the `format` argument here in
#'   the documentation for approaches that deal with this issue.
#' @note For the `format` argument, note that "long" was used instead of
#'   "unmodified" prior to v0.10.10.
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param format `character(1)`.
#'   Formatting method to apply:
#'
#'   - `"makeUnique"`: *Recommended.* Apply [`make.unique()`][base::make.unique]
#'     to the `geneName` column. Gene symbols are made unique, while the gene
#'     IDs remain unmodified.
#'   - `"unmodified"`: Return `geneID` and `geneName` columns unmodified, in
#'     long format.
#'   - `"1:1"`: For gene symbols that map to multiple gene IDs, select only the
#'     first annotated gene ID.
#'
#' @seealso [makeGene2Symbol()].
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Gene2Symbol(rse)
#' print(x)
NULL



## Updated 2019-07-22.
`Gene2Symbol,DataFrame` <-  # nolint
    function(object, format = c("makeUnique", "unmodified", "1:1")) {
        assert(hasRows(object))
        format <- match.arg(format)

        ## Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(sprintf(
                "Object does not contain gene-to-symbol mappings: %s.",
                toString(cols)
            ))
        }

        data <- DataFrame(
            geneID = as.character(decode(object[["geneID"]])),
            geneName = as.character(decode(object[["geneName"]])),
            row.names = rownames(object)
        )

        ## Inform the user about how many symbols multi-map.
        ## Note that `duplicated` doesn't work on Rle, so we have to coerce
        ## columns to character first (see `as_tibble` call above).
        duplicated <- duplicated(data[["geneName"]])
        if (any(duplicated)) {
            dupes <- unique(data[["geneName"]][duplicated])
            message(sprintf(
                "%d non-unique gene symbol(s) detected.", length(dupes)
            ))
        }

        if (format == "makeUnique") {
            ## Returning 1:1 mappings with renamed gene symbols.
            ## This is the default, and including a message is too noisy, since
            ## it is used heavily in other functions.
            data[["geneName"]] <- make.unique(data[["geneName"]])
        } else if (format == "unmodified") {
            message(
                "Returning with unmodified gene symbols ",
                "(may contain duplicates)."
            )
        } else if (format == "1:1") {
            message("Returning 1:1 mappings using oldest gene ID per symbol.")
            data <- data %>%
                as_tibble(rownames = NULL) %>%
                group_by(!!sym("geneName")) %>%
                arrange(!!sym("geneID"), .by_group = TRUE) %>%
                slice(n = 1L) %>%
                ungroup()
        }

        data <- as(data, "DataFrame")
        metadata(data) <- .slotGenomeMetadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Gene2Symbol", data)
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = `Gene2Symbol,DataFrame`
)



## Updated 2019-07-22.
`Gene2Symbol,GRanges` <-  # nolint
    function(object, format) {
        data <- as(object, "DataFrame")
        data <- unique(data)
        metadata(data) <- metadata(object)
        do.call(what = Gene2Symbol, args = list(object = data, format = format))
    }

formals(`Gene2Symbol,GRanges`) <- formals(`Gene2Symbol,DataFrame`)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = `Gene2Symbol,GRanges`
)



## Updated 2019-07-22.
`Gene2Symbol,SummarizedExperiment` <-  # nolint
    function(object, format) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(what = Gene2Symbol, args = list(object = data, format = format))
    }

formals(`Gene2Symbol,SummarizedExperiment`) <- formals(`Gene2Symbol,DataFrame`)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = `Gene2Symbol,SummarizedExperiment`
)



## HGNC2Ensembl ================================================================
#' @inherit HGNC2Ensembl-class title description return
#' @note Updated 2019-08-08.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert(hasInternet())

        if (isTRUE(getOption("acid.test"))) {
            file <- pasteURL(
                basejumpTestsURL, "hgnc.txt.gz",
                protocol = "none"
            )
        } else {
            ## This is unreliable on Travis, so cover locally instead.
            ## nocov start
            file <- pasteURL(
                "ftp.ebi.ac.uk",
                "pub",
                "databases",
                "genenames",
                "new",
                "tsv",
                "hgnc_complete_set.txt",
                protocol = "ftp"
            )
            ## nocov end
        }

        message("Obtaining HGNC to Ensembl gene ID mappings.")
        ## Note that this file does not contain syntactically valid names, and
        ## `readr::read_tsv()` has parsing issues with it.
        suppressWarnings(data <- import(file))
        data <- camel(data)
        data <- data[, c("hgncID", "ensemblGeneID")]
        colnames(data)[[2L]] <- "geneID"
        data <- data[!is.na(data[["geneID"]]), , drop = FALSE]
        data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
        data <- data[order(data[["hgncID"]]), , drop = FALSE]

        data <- as(data, "DataFrame")
        metadata(data) <- .prototypeMetadata
        new(Class = "HGNC2Ensembl", data)
    }



## MGI2Ensembl =================================================================
#' @inherit MGI2Ensembl-class title description return
#' @note Updated 2019-08-08.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- MGI2Ensembl()
#' print(x)
MGI2Ensembl <- function() {  # nolint
    assert(hasInternet())

    if (isTRUE(getOption("acid.test"))) {
        file <- pasteURL(basejumpTestsURL, "mgi.rpt.gz", protocol = "none")
    } else {
        file <- pasteURL(
            "www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            protocol = "http"
        )
    }

    message("Obtaining MGI-to-Ensembl gene ID mappings.")
    data <- read_tsv(
        file = file,
        ## Using our global NA strings.
        na = naStrings,
        col_names = FALSE,
        ## Suppress the column messages.
        col_types = cols(),
        skip = 1L,
        progress = FALSE
    )
    data <- as(data[, c(1L, 11L)], "DataFrame")
    colnames(data) <- c("mgiID", "geneID")
    data[["mgiID"]] <- as.integer(gsub("^MGI\\:", "", data[["mgiID"]]))
    data <- data[order(data[["mgiID"]]), , drop = FALSE]
    metadata(data) <- .prototypeMetadata

    new(Class = "MGI2Ensembl", data)
}



## PANTHER =====================================================================
#' @inherit PANTHER-class title description return
#' @note Updated 2019-08-08.
#' @export
#'
#' @section Suported organisms:
#'
#' - *Caenorhabditis elegans*
#' - *Drosophila melanogaster*
#' - *Homo sapiens*
#' - *Mus musculus*
#'
#' @inheritParams acidroxygen::params
#' @param release `character(1)` or `NULL`.
#'   PANTHER release version. If `NULL`, defaults to current release. Consult
#'   the PANTHER website for a list of release versions available from the FTP
#'   server (e.g. `"14.0"`).
#' @param progress `logical(1)`.
#'   Show progress.
#'   Uses `pbapply::pblapply()` internally to render progress bar.
#'
#' @examples
#' options(acid.test = TRUE)
#' x <- PANTHER("Homo sapiens", progress = FALSE)
#' summary(x)
PANTHER <- function(  # nolint
    organism,
    release = NULL,
    progress = getOption("acid.progress", default = FALSE)
) {
    assert(
        hasInternet(),
        isString(organism)
    )
    organism <- match.arg(
        arg = snake(organism),
        choices = names(.pantherMappings)
    )
    pantherName <-  .pantherMappings[[organism]]
    assert(isString(pantherName))
    if (is.null(release)) {
        release <- "current_release"
    }
    assert(
        isString(release),
        isFlag(progress)
    )
    release <- match.arg(arg = release, choices = .pantherReleases)

    message(sprintf(
        "Downloading PANTHER annotations for %s (%s).",
        organism, release
    ))

    if (isTRUE(getOption("acid.test"))) {
        file <- pasteURL(
            basejumpTestsURL, paste0("PTHR13.1_", pantherName, ".gz"),
            protocol = "none"
        )
    } else {
        ## This works unreliably on Travis, so cover locally instead.
        ## nocov start
        file <- transmit(
            remoteDir = pasteURL(
                "ftp.pantherdb.org",
                "sequence_classifications",
                release,
                "PANTHER_Sequence_Classification_files",
                protocol = "ftp"
            ),
            pattern = pantherName,
            compress = TRUE,
            localDir = tempdir()
        )
        ## nocov end
    }
    assert(isString(file))

    data <- read_tsv(
        file = file,
        col_names = c(
            "pantherID",
            "X2",
            "pantherSubfamilyID",
            "pantherFamilyName",
            "pantherSubfamilyName",
            "goMF",
            "goBP",
            "goCC",
            "pantherClass",
            "pantherPathway"
        ),
        col_types = cols(),
        progress = progress
    ) %>%
        separate(
            col = "pantherID",
            into = c("organism", "keys", "uniprotKB"),
            sep = "\\|"
        ) %>%
        mutate(
            X2 = NULL,
            organism = NULL,
            uniprotKB = NULL
        )

    ## Using organism-specific internal return functions here.
    fun <- get(paste("", "PANTHER", camel(organism), sep = "."))
    assert(is.function(fun))
    data <- fun(data)
    assert(hasRows(data))

    ## FIXME Switch away from dplyr here.
    data <- data %>%
        select(-!!sym("keys")) %>%
        select(!!sym("geneID"), everything()) %>%
        filter(!is.na(!!sym("geneID"))) %>%
        unique() %>%
        ## Some organisms have duplicate annotations per gene ID.
        group_by(!!sym("geneID")) %>%
        top_n(n = 1L, wt = !!sym("pantherSubfamilyID")) %>%
        ungroup() %>%
        arrange(!!sym("geneID"))
    assert(hasNoDuplicates(data[["geneID"]]))

    message("Splitting and sorting the GO terms.")
    data <- data %>%
        mutate_at(
            .vars = c(
                "goMF",
                "goBP",
                "goCC",
                "pantherClass",
                "pantherPathway"
            ),
            .funs = .splitTerms,
            progress = progress
        ) %>%
        ## Sort columns alphabetically.
        .[, sort(colnames(.)), drop = FALSE] %>%
        as("DataFrame") %>%
        set_rownames(.[["geneID"]])

    metadata(data) <- .prototypeMetadata
    metadata(data)[["organism"]] <- organism
    metadata(data)[["release"]] <- release

    new("PANTHER", data)
}



## Updated 2019-07-22.
.pantherMappings <- c(
    "caenorhabditis_elegans" = "nematode_worm",
    "drosophila_melanogaster" = "fruit_fly",
    "homo_sapiens" = "human",
    "mus_musculus" = "mouse"
)



## Release versions are here:
## ftp://ftp.pantherdb.org/sequence_classifications/
## Updated 2019-07-22.
.pantherReleases <- c(
    "11.0",
    "12.0",
    "13.0",
    "13.1",
    "14.0",
    "current_release"
)



## Updated 2019-07-22.
.PANTHER.homoSapiens <-  # nolint
    function(data) {
        hgnc2ensembl <- HGNC2Ensembl()

        ## Ensembl matches.
        ensembl <- data %>%
            mutate(
                geneID = str_extract(!!sym("keys"), "ENSG[0-9]{11}")
            ) %>%
            filter(!is.na(!!sym("geneID")))

        ## HGNC matches.
        hgnc <- data %>%
            as_tibble() %>%
            ## Extract the HGNC ID.
            mutate(
                hgncID = str_match(!!sym("keys"), "HGNC=([0-9]+)")[, 2L],
                hgncID = as.integer(!!sym("hgncID"))
            ) %>%
            filter(!is.na(!!sym("hgncID"))) %>%
            left_join(
                as_tibble(hgnc2ensembl, rownames = NULL),
                by = "hgncID"
            ) %>%
            select(-!!sym("hgncID")) %>%
            filter(!is.na(!!sym("geneID"))) %>%
            unique()

        do.call(rbind, list(ensembl, hgnc))
    }



## Updated 2019-07-22.
.PANTHER.musMusculus <-  # nolint
    function(data) {
        mgi2ensembl <- MGI2Ensembl()

        ## Ensembl matches.
        ensembl <- data %>%
            mutate(
                geneID = str_extract(!!sym("keys"), "ENSMUSG[0-9]{11}")
            ) %>%
            filter(!is.na(!!sym("geneID")))

        ## MGI matches.
        mgi <- data %>%
            as_tibble() %>%
            mutate(
                mgiID = str_match(!!sym("keys"), "MGI=([0-9]+)")[, 2L],
                mgiID = as.integer(!!sym("mgiID"))
            ) %>%
            filter(!is.na(!!sym("mgiID"))) %>%
            left_join(
                as_tibble(mgi2ensembl, rownames = NULL),
                by = "mgiID"
            ) %>%
            select(-!!sym("mgiID")) %>%
            filter(!is.na(!!sym("geneID")))

        do.call(rbind, list(ensembl, mgi))
    }



## Updated 2019-07-22.
.PANTHER.drosophilaMelanogaster <-  # nolint
    function(data) {
        mutate(data, geneID = str_extract(!!sym("keys"), "FBgn\\d{7}$"))
    }



## Updated 2019-07-22.
.PANTHER.caenorhabditisElegans <-  # nolint
    function(data) {
        mutate(data, geneID = str_extract(!!sym("keys"), "WBGene\\d{8}$"))
    }



## This step is CPU intensive, so optionally enable progress bar.
## Alternatively, consider switching to BiocParallel bpparam usage here.
## Updated 2019-07-22.
.splitTerms <- function(x, progress = FALSE) {
    if (isTRUE(progress)) {
        ## nocov start
        message(deparse(substitute(x)))
        requireNamespace("pbapply", quietly = TRUE)
        lapply <- pbapply::pblapply
        ## nocov end
    }
    lapply(x, function(x) {
        x <- x %>%
            as.character() %>%
            strsplit(split = ";") %>%
            unlist() %>%
            unique() %>%
            sort() %>%
            gsub("#([A-Z0-9:]+)", " [\\1]", .) %>%
            gsub(">", " > ", .)
        if (length(x) > 0L) {
            x
        } else {
            NULL
        }
    })
}



## Tx2Gene =====================================================================
#' @inherit Tx2Gene-class title description return
#' @name Tx2Gene
#'
#' @note No attempt is made to arrange the rows by transcript identifier.
#' @note Updated 2019-08-13.
#'
#' @inheritParams acidroxygen::params
#'
#' @seealso [makeTx2Gene()].
#'
#' @examples
#' ## SummarizedExperiment ====
#' data(SummarizedExperiment_transcripts, package = "acidtest")
#' txse <- SummarizedExperiment_transcripts
#'
#' ## SummarizedExperiment ====
#' x <- Tx2Gene(txse)
#' print(x)
NULL



## Updated 2019-08-13.
`Tx2Gene,DataFrame` <-  # nolint
    function(object) {
        assert(hasRows(object))
        ## Check for required columns.
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(sprintf(
                "Object does not contain transcript-to-gene mappings: %s.",
                toString(cols)
            ))
        }
        transcriptID <- as.character(decode(object[["transcriptID"]]))
        geneID <- as.character(decode(object[["geneID"]]))
        rownames <- rownames(object)
        if (is.null(rownames)) {
            rownames <- transcriptID
        }
        data <- DataFrame(
            transcriptID = transcriptID,
            geneID = geneID,
            row.names = rownames
        )
        metadata(data) <- .slotGenomeMetadata(object)
        new(Class = "Tx2Gene", data)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("DataFrame"),
    definition = `Tx2Gene,DataFrame`
)



## Updated 2019-07-22.
`Tx2Gene,GRanges` <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        ## This step is needed for handling raw GFF annotations.
        data <- unique(data)
        metadata(data) <- metadata(object)
        Tx2Gene(data)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("GRanges"),
    definition = `Tx2Gene,GRanges`
)



## Updated 2019-07-22.
`Tx2Gene,SummarizedExperiment` <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(what = Tx2Gene, args = list(object = data))
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = `Tx2Gene,SummarizedExperiment`
)
