# checkClasses =================================================================
# TODO Move this to assertions?
#' Check Classes
#'
#' @export
#'
#' @inheritParams basejump.globals::params
#' @param expected `list`. Named list of expected classes per slot.
#' @param subset `boolean`. Only check a subset of slots in the object.
#'
#' @examples
#' data(rse, package = "basejump.data")
#' object <- rse
#' metadata <- S4Vectors::metadata(object)
#' checkClasses(
#'     object = metadata,
#'     expected = list(
#'         version = c("package_version", "numeric_version"),
#'         date = "Date",
#'         interestingGroups = "character"
#'     )
#' )
checkClasses <- function(
    object,
    expected,
    subset = FALSE
) {
    assert_is_list(expected)
    assert_has_names(expected)
    assert_is_a_bool(subset)
    if (isTRUE(subset)) {
        assert_is_subset(names(expected), names(object))
    } else {
        assert_are_set_equal(names(expected), names(object))
    }
    valid <- mapply(
        slot = names(expected),
        classes = expected,
        MoreArgs = list(object = object),
        FUN = function(slot, classes, object) {
            intersect <- intersect(
                x = classes,
                y = class(object[[slot]])
            )
            if (!has_length(intersect)) {
                FALSE
            } else {
                TRUE
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    if (!all(valid)) {
        stop(paste(
            "Class checks failed.",
            "Run `updateObject()` to update your object.",
            printString(valid),
            sep = "\n"
        ))
    }
    TRUE
}



# EggNOG =======================================================================
#' EggNOG Database Annotations
#'
#' [EggNOG](http://eggnogdb.embl.de) is a database of biological information
#' hosted by the EMBL. It is based on the original idea of COGs (**c**lusters of
#' **o**rthologous **g**roups) and expands that idea to non-supervised
#' orthologous groups constructed from numerous organisms. eggNOG stands for
#' **e**volutionary **g**enealogy of **g**enes: **N**on-supervised
#' **O**rthologous **G**roups.
#'
#' @author Michael Steinbaugh
#' @inherit EggNOG
#' @export
#'
#' @return `EggNOG`. This class extends `list` and contains:
#'
#' 1. "`cogFunctionalCategories`": **C**luster of **O**rthologous **G**roups
#'    (COG) functional category information.
#' 2. "`annotations`": up-to-date functional descriptions and categories
#'    for **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG)
#'    and **N**on-supervised **O**rthologous **G**roups (NOG) protein
#'    identifiers.
#'
#' The [EggNOG README file](http://eggnogdb.embl.de/download/latest/README.txt)
#' contains additional useful reference information.
setClass(Class = "EggNOG", contains = "SimpleDataFrameList")



setValidity(
    Class = "EggNOG",
    method = function(object) {
        .valid(list(
            validate_that(identical(
                x = names(object),
                y = c("cogFunctionalCategories", "annotations")
            )),
            validate_that(identical(
                x = colnames(object[["cogFunctionalCategories"]]),
                y = c("letter", "description")
            )),
            validate_that(identical(
                x = colnames(object[["annotations"]]),
                y = c(
                    "eggnogID",
                    "consensusFunctionalDescription",
                    "cogFunctionalCategory"
                )
            ))
        ))
    }
)



#' @inherit EggNOG-class
#' @inheritParams basejump.globals::params
#' @export
#' @examples
#' options(basejump.test = TRUE)
#' x <- EggNOG()
#' print(x)
EggNOG <-  # nolint
    function() {
        assert_that(has_internet())
        if (isTRUE(getOption("basejump.test"))) {
            categoriesFile <- file.path(basejumpCacheURL, "cog.txt")
            eunogFile <- file.path(basejumpCacheURL, "eunog.tsv.gz")
            nogFile <- file.path(basejumpCacheURL, "nog.tsv.gz")
        } else {
            url <- paste(
                "http://eggnogdb.embl.de",
                "download",
                "latest",
                sep = "/"
            )
            categoriesFile <- paste(
                url,
                "COG_functional_categories.txt",
                sep = "/"
            )
            eunogFile <- paste(
                url,
                "data",
                "euNOG",
                "euNOG.annotations.tsv.gz",
                sep = "/"
            )
            nogFile <- paste(
                url,
                "data",
                "NOG",
                "NOG.annotations.tsv.gz",
                sep = "/"
            )
        }
        assert_is_a_string(categoriesFile)
        assert_is_a_string(eunogFile)
        assert_is_a_string(nogFile)

        # Categories -----------------------------------------------------------
        pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
        categories <- read_lines(file = categoriesFile) %>%
            str_subset(pattern) %>%
            str_match(pattern) %>%
            as_tibble() %>%
            select(-1L) %>%
            set_colnames(c("letter", "description")) %>%
            arrange(!!sym("letter")) %>%
            as("DataFrame")

        # Annotations ----------------------------------------------------------
        colnames <- c(
            "taxonomicLevel",
            "groupName",
            "proteinCount",
            "speciesCount",
            "cogFunctionalCategory",
            "consensusFunctionalDescription"
        )

        # euNOG: Eukaryota
        eunog <- read_tsv(
            file = eunogFile,
            col_names = colnames,
            col_types = cols(),
            progress = FALSE
        )

        # NOG: LUCA
        nog <- read_tsv(
            file = nogFile,
            col_names = colnames,
            col_types = cols(),
            progress = FALSE
        )

        annotations <- bind_rows(eunog, nog) %>%
            select(!!!syms(c(
                "groupName",
                "consensusFunctionalDescription",
                "cogFunctionalCategory"
            ))) %>%
            rename(eggnogID = !!sym("groupName")) %>%
            arrange(!!sym("eggnogID")) %>%
            as("DataFrame")

        # Return ---------------------------------------------------------------
        data <- List(
            cogFunctionalCategories = categories,
            annotations = annotations
        )
        metadata(data) <- .prototypeMetadata
        new(Class = "EggNOG", data)
    }



# Ensembl2Entrez ===============================================================
#' Ensembl-to-Entrez Gene Identifier Mappings
#'
#' Defines 1:1 mappings from Ensembl gene IDs to Entrez IDs. Uses the oldest
#' Entrez ID if there are multiple identifiers that map to an Ensembl gene ID.
#'
#' @author Michael Steinbaugh
#' @inherit Ensembl2Entrez
#' @export
#'
#' @return `Ensembl2Entrez`. Contains a `DataFrame` with `geneID` and `entrezID`
#'   columns.
setClass(Class = "Ensembl2Entrez", contains = "DataFrame")



setValidity(
    Class = "Ensembl2Entrez",
    method = function(object) {
        .valid(list(
            validate_that(identical(
                x = colnames(object),
                y = c("geneID", "entrezID")
            )),
            validate_that(!any(duplicated(object[["geneID"]]))),
            validate_that(
                class(object[["entrezID"]]) %in% c("integer", "list")
            )
        ))
    }
)



#' @name Ensembl2Entrez
#' @inherit Ensembl2Entrez-class
#' @inheritParams basejump.globals::params
#'
#' @param format `string`. Formatting method to apply:
#' - `"1:1"`: *Recommended.* Return with 1:1 mappings. For Ensembl genes that
#'   don't map 1:1 with Entrez, pick the oldest Entrez ID. Genes that don't map
#'   to Entrez will contain `NA` in `entrezID` column.
#' - `"long"`: Return 1:many in long format.
#'
#' @examples
#' data(rse, package = "basejump.data")
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



Ensembl2Entrez.DataFrame <-  # nolint
    function(
        object,
        format = c("1:1", "long")
    ) {
        assert_has_rows(object)
        format <- match.arg(format)

        cols <- c("geneID", "entrezID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain Ensembl-to-Entrez mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object[, cols, drop = FALSE]

        # Expand to long format.
        data <- expand(data)

        # Inform the user about genes that don't map to Entrez.
        unmapped <- data[["geneID"]][which(is.na(data[["entrezID"]]))]
        assert_that(!any(duplicated(unmapped)))
        if (has_length(unmapped)) {
            message(paste(length(unmapped), "genes don't map to Entrez."))
        }

        # Inform the user about how many genes multi-map to Entrez.
        multimapped <- unique(data[["geneID"]][duplicated(data[["geneID"]])])
        if (has_length(multimapped)) {
            message(paste(
                length(multimapped), "genes map to multiple Entrez IDs."
            ))
        }

        if (format == "1:1") {
            message(paste(
                "Returning with 1:1 mappings using oldest Entrez ID per gene."
            ))
            entrez <- object[["entrezID"]]
            assert_is_list(entrez)
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
            message("Returning 1:many in long format.")
        }

        metadata(data) <- metadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Ensembl2Entrez", data)
    }



Ensembl2Entrez.GRanges <-  # nolint
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
formals(Ensembl2Entrez.GRanges) <- formals(Ensembl2Entrez.DataFrame)



Ensembl2Entrez.SummarizedExperiment <-  # nolint
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
formals(Ensembl2Entrez.SummarizedExperiment) <-
    formals(Ensembl2Entrez.DataFrame)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("DataFrame"),
    definition = Ensembl2Entrez.DataFrame
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("GRanges"),
    definition = Ensembl2Entrez.GRanges
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("SummarizedExperiment"),
    definition = Ensembl2Entrez.SummarizedExperiment
)



# Gene2Symbol ==================================================================
#' Gene-to-Symbol Mappings
#'
#' @note For some organisms, gene names and gene symbols do not map 1:1
#' (e.g. *Homo sapiens* and *Mus musculus*). Refer to the `format` argument here
#' in the documentation for approaches that deal with this issue.
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' `S4Vectors::metadata()`.
#'
#' @author Michael Steinbaugh
#' @inherit Gene2Symbol
#' @export
#'
#' @seealso `help(topic = "makeGene2Symbol", package = "basejump.annotations")`.
#'
#' @return `Gene2Symbol`. Contains a `DataFrame` with `geneID` and `geneName`
#'   columns.
setClass(Class = "Gene2Symbol", contains = "DataFrame")



setValidity(
    Class = "Gene2Symbol",
    method = function(object) {
        .valid(list(
            validate_that(identical(
                x = colnames(object),
                y = c("geneID", "geneName")
            )),
            validate_that(nrow(object) > 0L),
            validate_that(is.character(object[["geneID"]])),
            validate_that(
                class(object[["geneName"]]) %in% c("character", "factor")
            )
        ))
    }
)



#' @name Gene2Symbol
#' @inherit Gene2Symbol-class
#' @inheritParams basejump.globals::params
#'
#' @param format `string`. Formatting method to apply:
#' - `"makeUnique"`: *Recommended.* Apply [base::make.unique()] to the
#'   `geneName` column. Gene symbols are made unique, while the gene IDs remain
#'   unmodified.
#' - `"1:1"`: For gene symbols that map to multiple gene IDs, select only the
#'   first annotated gene ID.
#' - `"long"`: Return `geneID` and `geneName` columns unmodified in long format.
#'
#' @examples
#' data(rse, package = "basejump.data")
#' x <- Gene2Symbol(rse)
#' print(x)
NULL



Gene2Symbol.DataFrame <-  # nolint
    function(
        object,
        format = c("makeUnique", "1:1", "long")
    ) {
        assert_has_rows(object)
        format <- match.arg(format)

        # Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain gene-to-symbol mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object[, cols, drop = FALSE]

        # Inform the user about how many symbols multi-map.
        duplicated <- duplicated(data[["geneName"]])
        if (any(duplicated)) {
            dupes <- unique(data[["geneName"]][duplicated])
            message(paste(
                length(dupes), "non-unique gene symbol(s) detected."
            ))
        }

        if (format == "makeUnique") {
            message("Returning 1:1 mappings with renamed gene symbols.")
            data[["geneName"]] <- data[["geneName"]] %>%
                as.character() %>%
                make.unique()
        } else if (format == "1:1") {
            message("Returning 1:1 mappings using oldest gene ID per symbol.")
            data <- data %>%
                as_tibble(rownames = NULL) %>%
                mutate_all(as.character) %>%
                group_by(!!sym("geneName")) %>%
                arrange(!!!sym("geneID"), .by_group = TRUE) %>%
                slice(n = 1L) %>%
                ungroup()
        } else if (format == "long") {
            message("Returning mappings in long format.")
        }

        data <- as(data, "DataFrame")
        metadata(data) <- .genomeMetadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Gene2Symbol", data)
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = Gene2Symbol.DataFrame
)



Gene2Symbol.GRanges <-  # nolint
    function(object, format) {
        data <- as(object, "DataFrame")
        data <- unique(data)
        metadata(data) <- metadata(object)
        do.call(
            what = Gene2Symbol,
            args = list(
                object = data,
                format = format
            )
        )
    }
formals(Gene2Symbol.GRanges) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = Gene2Symbol.GRanges
)



Gene2Symbol.SummarizedExperiment <-  # nolint
    function(object, format) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(
            what = Gene2Symbol,
            args = list(
                object = data,
                format = format
            )
        )
    }
formals(Gene2Symbol.SummarizedExperiment) <- formals(Gene2Symbol.DataFrame)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = Gene2Symbol.SummarizedExperiment
)



# HGNC2Ensembl =================================================================
#' HGNC-to-Ensembl Gene Identifier Mappings
#'
#' @author Michael Steinbaugh
#' @inherit HGNC2Ensembl
#' @export
#'
#' @return `HGNC2Ensembl`. Contains a `DataFrame` with `hgncID` and `geneID`
#'   columns.
setClass(Class = "HGNC2Ensembl", contains = "DataFrame")



setValidity(
    Class = "HGNC2Ensembl",
    method = function(object) {
        .valid(list(
            validate_that(identical(
                x = lapply(object, class),
                y = list(
                    hgncID = "integer",
                    geneID = "character"
                )
            )),
            validate_that(identical(
                x = rownames(object),
                y = NULL
            ))
        ))
    }
)



#' @inherit HGNC2Ensembl-class
#' @inheritParams basejump.globals::params
#' @export
#' @examples
#' options(basejump.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert_that(has_internet())

        if (isTRUE(getOption("basejump.test"))) {
            file <- file.path(basejumpCacheURL, "hgnc.txt.gz")
        } else {
            file <- paste(
                "ftp://ftp.ebi.ac.uk",
                "pub",
                "databases",
                "genenames",
                "new",
                "tsv",
                "hgnc_complete_set.txt",
                sep = "/"
            )
        }

        message("Obtaining HGNC to Ensembl gene ID mappings.")
        data <- read_tsv(
            file = file,
            # Suppress the column messages.
            col_types = cols(),
            progress = FALSE
        )
        data <- camel(data)
        data <- data[, c("hgncID", "ensemblGeneID")]
        colnames(data)[[2L]] <- "geneID"
        data <- data[!is.na(data[["geneID"]]), ]
        data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
        data <- data[order(data[["hgncID"]]), ]
        data <- as(data, "DataFrame")
        metadata(data) <- .prototypeMetadata

        new(Class = "HGNC2Ensembl", data)
    }



# MGI2Ensembl ==================================================================
#' MGI-to-Ensembl Gene Identifier Mappings
#'
#' @author Michael Steinbaugh
#' @inherit MGI2Ensembl
#' @export
#'
#' @return `MGI2Ensembl`. Contains a `DataFrame` with `mgiID` and `geneID`
#'   columns.
setClass(Class = "MGI2Ensembl", contains = "DataFrame")



setValidity(
    Class = "MGI2Ensembl",
    method = function(object) {
        .valid(list(
            validate_that(identical(
                x = colnames(object),
                y = c("mgiID", "geneID")
            )),
            validate_that(is.null(rownames(object)))
        ))
    }
)



#' @inherit MGI2Ensembl-class
#'
#' @export
#'
#' @inheritParams basejump.globals::params
#'
#' @return `MGI2Ensembl`.
#'
#' @examples
#' options(basejump.test = TRUE)
#' x <- MGI2Ensembl()
#' print(x)
MGI2Ensembl <- function() {  # nolint
    assert_that(has_internet())

    if (isTRUE(getOption("basejump.test"))) {
        file <- file.path(basejumpCacheURL, "mgi.rpt.gz")
    } else {
        file <- paste(
            "http://www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            sep = "/"
        )
    }

    message("Obtaining MGI to Ensembl gene ID mappings.")
    data <- read_tsv(
        file = file,
        # Using our global NA strings.
        na = naStrings,
        col_names = FALSE,
        # Suppress the column messages.
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



# PANTHER ======================================================================
#' PANTHER Database Annotations
#'
#' [PANTHER](http://www.pantherdb.org) gene ontology definitions. PANTHER stands
#' for **P**rotein **AN**alysis **TH**rough **E**volutionary **R**elationships.
#'
#' @author Michael Steinbaugh
#' @inherit PANTHER
#' @export
#'
#' @return `PANTHER`. Contains a `DataFrame`.
setClass(Class = "PANTHER", contains = "DataFrame")



setValidity(
    Class = "PANTHER",
    method = function(object) {
        .valid(list(
            validate_that(identical(
                x = colnames(object),
                y = c(
                    "geneID",
                    "goBP",
                    "goCC",
                    "goMF",
                    "pantherClass",
                    "pantherFamilyName",
                    "pantherPathway",
                    "pantherSubfamilyID",
                    "pantherSubfamilyName"
                )
            )),
            validate_that(
                all(c("organism", "release") %in% names(metadata(object)))
            )
        ))
    }
)



#' @name PANTHER
#' @inherit PANTHER-class
#' @inheritParams basejump.globals::params
#'
#' @param organism `string`. Full Latin organism name. Currently supported
#'   organisms: *Homo sapiens*, *Mus musculus*, *Caenorhabditis elegans*,
#'   *Drosophila melanogaster*.
#' @param release `string` or `NULL`. PANTHER release version. If set `NULL`,
#'   defaults to current release. Consult the PANTHER website for a list of
#'   release versions available from the FTP server (e.g. `"13.0"`).
#' @param progress `boolean`. Use [pbapply::pblapply()] internally to show
#'   progress.
#'
#' @examples
#' options(basejump.test = TRUE)
#' x <- PANTHER("Homo sapiens", progress = FALSE)
#' summary(x)
NULL



.pantherMappings <- c(
    "Homo sapiens" = "human",
    "Mus musculus" = "mouse",
    "Caenorhabditis elegans" = "nematode_worm",
    "Drosophila melanogaster" = "fruit_fly"
)



.splitTerms <- function(vec, progress = FALSE) {
    if (isTRUE(progress)) {
        message(deparse(substitute(vec)))
        lapply <- pblapply
    }
    lapply(vec, function(x) {
        x <- x %>%
            strsplit(split = ";") %>%
            unlist() %>%
            unique() %>%
            sort() %>%
            gsub("#([A-Z0-9:]+)", " [\\1]", .) %>%
            gsub(">", " > ", .)
        if (has_length(x)) {
            x
        } else {
            NULL
        }
    })
}



.PANTHER.homoSapiens <-  # nolint
    function(data) {
        hgnc2ensembl <- HGNC2Ensembl()

        # Ensembl matches.
        ensembl <- data %>%
            mutate(
                geneID = str_extract(!!sym("keys"), "ENSG[0-9]{11}")
            ) %>%
            filter(!is.na(!!sym("geneID")))

        # HGNC matches.
        hgnc <- data %>%
            as_tibble() %>%
            # Extract the HGNC ID.
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



.PANTHER.musMusculus <-  # nolint
    function(data) {
        mgi2ensembl <- MGI2Ensembl()

        # Ensembl matches.
        ensembl <- data %>%
            mutate(
                geneID = str_extract(!!sym("keys"), "ENSMUSG[0-9]{11}")
            ) %>%
            filter(!is.na(!!sym("geneID")))

        # MGI matches.
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



.PANTHER.drosophilaMelanogaster <-  # nolint
    function(data) {
        mutate(data, geneID = str_extract(!!sym("keys"), "FBgn\\d{7}$"))
    }



.PANTHER.caenorhabditisElegans <-  # nolint
    function(data) {
        mutate(data, geneID = str_extract(!!sym("keys"), "WBGene\\d{8}$"))
    }



#' @rdname PANTHER
#' @export
PANTHER <- function(  # nolint
    organism,
    release = NULL,
    progress = FALSE
) {
    assert_that(has_internet())
    organism <- match.arg(
        arg = organism,
        choices = names(.pantherMappings)
    )
    pantherName <-  .pantherMappings[[organism]]
    assert_is_a_string(pantherName)
    if (is.null(release)) {
        release <- "current_release"
    }
    assert_is_a_string(release)
    assert_is_a_bool(progress)

    message(paste0(
        "Downloading PANTHER annotations for ",
        organism,
        " (", release, ")."
    ))

    if (isTRUE(getOption("basejump.test"))) {
        file <- file.path(
            basejumpCacheURL,
            paste0("PTHR13.1_", pantherName, ".gz")
        )
    } else {
        file <- transmit(
            remoteDir = paste(
                "ftp://ftp.pantherdb.org",
                "sequence_classifications",
                release,
                "PANTHER_Sequence_Classification_files",
                sep = "/"
            ),
            pattern = pantherName,
            compress = TRUE,
            localDir = tempdir()
        )
    }
    assert_is_a_string(file)

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

    # Using organism-specific internal return functions here.
    fun <- get(paste("", "PANTHER", camel(organism), sep = "."))
    assert_is_function(fun)
    data <- fun(data)
    assert_has_rows(data)

    data <- data %>%
        select(-!!sym("keys")) %>%
        select(!!sym("geneID"), everything()) %>%
        filter(!is.na(!!sym("geneID"))) %>%
        unique() %>%
        # Some organisms have duplicate annotations per gene ID.
        group_by(!!sym("geneID")) %>%
        top_n(n = 1L, wt = !!sym("pantherSubfamilyID")) %>%
        ungroup() %>%
        arrange(!!sym("geneID"))
    assert_has_no_duplicates(data[["geneID"]])

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
        # Sort columns alphabetically.
        .[, sort(colnames(.)), drop = FALSE] %>%
        as("DataFrame") %>%
        set_rownames(.[["geneID"]])

    metadata(data) <- .prototypeMetadata
    metadata(data)[["organism"]] <- organism
    metadata(data)[["release"]] <- release

    new("PANTHER", data)
}



# Tx2Gene ======================================================================
#' Transcript-to-Gene Identifier Mappings
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' `S4Vectors::metadata()`.
#'
#' @author Michael Steinbaugh
#' @inherit Tx2Gene
#' @export
#'
#' @seealso `help(topic = "makeTx2Gene", package = "basejump.annotations")`.
#'
#' @return `Tx2Gene`. Contains a `DataFrame` with `transcriptID` and `geneID`
#'   columns.
setClass(Class = "Tx2Gene", contains = "DataFrame")



setValidity(
    Class = "Tx2Gene",
    method = function(object) {
        .valid(list(
            validate_that(nrow(object) > 0L),
            validate_that(identical(
                x = colnames(object),
                y = c("transcriptID", "geneID")
            )),
            validate_that(all(vapply(
                X = object,
                FUN = is.character,
                FUN.VALUE = logical(1L)
            ))),
            validate_that(!any(duplicated(object[["transcriptID"]])))
        ))
    }
)



#' @name Tx2Gene
#' @inherit Tx2Gene-class
#' @inheritParams basejump.globals::params
#' @note No attempt is made to arrange the rows by transcript identifier.
#' @examples
#' ## SummarizedExperiment ====
#' data(tx_se, package = "basejump.data")
#' x <- Tx2Gene(tx_se)
#' print(x)
NULL



Tx2Gene.DataFrame <-  # nolint
    function(object) {
        assert_has_rows(object)

        # Check for required columns.
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain transcript-to-gene mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object %>%
            .[, cols, drop = FALSE] %>%
            as("tbl_df") %>%
            mutate_all(as.character) %>%
            as("DataFrame")

        metadata(data) <- .genomeMetadata(object)
        new(Class = "Tx2Gene", data)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("DataFrame"),
    definition = Tx2Gene.DataFrame
)



Tx2Gene.GRanges <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        # This step is needed for handling raw GFF annotations.
        data <- unique(data)
        metadata(data) <- metadata(object)
        Tx2Gene(data)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("GRanges"),
    definition = Tx2Gene.GRanges
)



Tx2Gene.SummarizedExperiment <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(
            what = Tx2Gene,
            args = list(object = data)
        )
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = Tx2Gene.SummarizedExperiment
)

