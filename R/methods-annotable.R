#' Ensembl Annotations
#'
#' Quickly obtain gene annotations from [Ensembl](http://www.ensembl.org/).
#'
#' @section Using AnnotationHub:
#' The default recommended approach is to simply specify the desired organism,
#' using the full latin name. For example, we can obtain human annotations with
#' `Homo sapiens` and mouse annotations with `Mus musculus`. Under the hood, the
#' function fetches these annotations from AnnotationHub using the ensembldb
#' package. AnnotationHub supports versioned Ensembl releases, back to version
#' 87.
#'
#' @section Using Pre-Compiled Annotables:
#' Generally we' don't recommend this approach. However, pass-in of a tibble
#' generated using [biomaRt](https://doi.org/doi:10.18129/B9.bioc.biomaRt) in
#' the [annotables](https://github.com/stephenturner/annotables) data package is
#' useful for adding support for the outdated (but still frequently used)
#' GRCh37/hg19 human genome.
#'
#' @section Broad Class Definitions:
#' A `broadClass` column is added, which generalizes the gene types into a
#' smaller number of semantically-meaningful groups:
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
#' @rdname annotable
#' @name annotable
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param object Object. Default recommended usage is to provide full latin
#'   organism name as a string. Also supports input of pre-built tibbles from
#'   the annotables data package.
#' @param format Desired table format, either `gene`, `tx2gene`, or
#'   `gene2symbol`.
#' @param genomeBuild *Optional.* Check to see if a specific genome build is
#'   supported.
#' @param release *Optional.* Ensembl release version. Defaults to the most
#'   current release available on AnnotationHub.
#' @param uniqueSymbol Make gene symbols unique.
#'
#' @return [data.frame] with unique rows per gene or transcript.
#'
#' @seealso
#' - [AnnotationHub](https://doi.org/doi:10.18129/B9.bioc.AnnotationHub).
#' - [ensembldb](https://doi.org/doi:10.18129/B9.bioc.ensembldb).
#'
#' @examples
#' annotable("Homo sapiens") %>% glimpse()
#'
#' # Legacy GRCh37/hg19 genome build support
#' annotable("Homo sapiens", genomeBuild = "GRCh37") %>% glimpse()
NULL



# Constructors =================================================================
#' @importFrom AnnotationHub AnnotationHub getAnnotationHubOption query
#'   snapshotDate
#' @importFrom BiocGenerics organism
#' @importFrom dplyr mutate rename
#' @importFrom ensembldb ensemblVersion genes transcripts
#' @importFrom rlang .data
#' @importFrom S4Vectors mcols
#' @importFrom utils capture.output find tail
.annotable.character <- function(  # nolint
    object,
    format = "gene",
    genomeBuild = NULL,
    release = NULL,
    uniqueSymbol = FALSE,
    quiet = FALSE) {
    assert_is_a_string(object)
    assert_is_a_string(format)
    assert_is_subset(format, c("gene", "gene2symbol", "tx2gene"))
    assert_is_a_string_or_null(genomeBuild)
    assert_is_an_implicit_integer_or_null(release)
    if (is.numeric(release)) {
        # AnnotableHub only supports releases 87 and above
        assert_all_are_greater_than_or_equal_to(release, 87L)
    }
    assert_is_a_bool(uniqueSymbol)
    assert_is_a_bool(quiet)

    # Ensure `select()` isn't masked by ensembldb/AnnotationDbi
    userAttached <- .packages()

    # Genome build =============================================================
    if (is_a_string(genomeBuild)) {
        # GRCh37/hg19 legacy support
        if (object == "Homo sapiens" &&
            grepl(
                pattern = paste("GRCh37", "hg19", sep = "|"),
                x = genomeBuild,
                ignore.case = TRUE)
        ) {
            if (format == "gene") {
                data <- load(system.file(
                    file.path("extdata", "grch37.rda"),
                    package = "basejump"
                ))
                data <- get(data, inherits = FALSE)
                return(data)
            } else if (format == "gene2symbol") {
                data <- load(system.file(
                    file.path("extdata", "grch37.rda"),
                    package = "basejump"
                ))
                data <- get(data, inherits = FALSE)
                data <- data[, c("ensgene", "symbol")]
                return(data)
            } else if (format == "tx2gene") {
                data <- load(system.file(
                    file.path("extdata", "grch37Tx2gene.rda"),
                    package = "basejump"
                ))
                data <- get(data, inherits = FALSE)
                return(data)
            }
        }
    }

    # Release version ==========================================================
    # Define the `releasePattern` to query with ensembldb
    if (is.null(release)) {
        releasePattern <- NULL
    } else {
        releasePattern <- paste0("v", release)
    }

    # AnnotationHub ============================================================
    # Connect to AnnotationHub. On a fresh install this will print a
    # txProgressBar to the console. We're using `capture.output()` here
    # to suppress the console output, since it's not very informative and
    # can cluster R Markdown reports.
    invisible(capture.output(
        ah <- suppressMessages(AnnotationHub())
    ))

    if (!isTRUE(quiet)) {
        inform(paste(
            "Loading Ensembl annotations from AnnotationHub",
            snapshotDate(ah),
            sep = "\n"
        ))
    }

    # Get the AnnotationHub dataset by identifier number
    ahDb <- query(
        ah,
        pattern = c(object, "EnsDb", releasePattern),
        ignore.case = TRUE)
    # Get the latest build version
    id <- ahDb %>%
        mcols() %>%
        rownames() %>%
        tail(n = 1L)

    # Abort on organism failure
    if (!length(id)) {
        abort(paste(
            "Full latin organism name", object,
            "is not supported in AnnotationHub"
        ))
    }

    # ensembldb ================================================================
    # This step will also output `txProgressBar()` on a fresh install. Using
    # `capture.output()` here again to suppress console output. Additionally, it
    # attaches ensembldb and other Bioconductor dependency packages, which will
    # mask some tidyverse functions (e.g. `select()`).
    invisible(capture.output(
        edb <- suppressMessages(ah[[id]])
    ))

    if (!isTRUE(quiet)) {
        inform(paste(
            "EnsDB", paste0(id, ":"),
            organism(edb),
            "Ensembl", ensemblVersion(edb)
        ))
    }

    # Now we can force detach ensembldb and other unwanted dependendcies from
    # the search path
    ensembldbAttached <- setdiff(.packages(), userAttached)
    invisible(lapply(
        X = ensembldbAttached,
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
    # Assert that all AnnotationHub/ensembldb packages must detach
    assert_are_identical(.packages(), userAttached)

    # Sanitize return ==========================================================
    if (format == "gene") {
        data <- genes(edb, return.type = "data.frame") %>%
            rename(
                ensgene = .data[["gene_id"]],
                biotype = .data[["gene_biotype"]]) %>%
            # Use `symbol` column instead of duplicate `gene_name`
            mutate(gene_name = NULL) %>%
            # Ensure rows are sorted by gene ID
            arrange(!!sym("ensgene"))
        if (isTRUE(uniqueSymbol)) {
            # Ensure unique symbols (e.g. human, mouse)
            data[["symbol"]] <- make.unique(data[["symbol"]], sep = ".")
        }
        # Sanitize using data frame method
        data <- annotable(data)
    } else if (format == "gene2symbol") {
        data <- genes(
            edb,
            columns = c("gene_id", "symbol"),
            return.type = "data.frame") %>%
            rename(ensgene = .data[["gene_id"]]) %>%
            # Ensure rows are sorted by gene ID
            arrange(!!sym("ensgene"))
        if (isTRUE(uniqueSymbol)) {
            # Ensure unique symbols (e.g. human, mouse)
            data[["symbol"]] <- make.unique(data[["symbol"]], sep = ".")
        }
        rownames(data) <- data[["ensgene"]]
    } else if (format == "tx2gene") {
        data <- transcripts(
            edb,
            columns = c("tx_id", "gene_id"),
            return.type = "data.frame") %>%
            rename(
                enstxp = .data[["tx_id"]],
                ensgene = .data[["gene_id"]]) %>%
            arrange(!!sym("enstxp")) %>%
            set_rownames(.[["enstxp"]])
    }
    assert_is_data.frame(data)
    assert_has_rownames(data)
    data
}



#' Define Broad Class
#'
#' @author Broad class definitions by Rory Kirchner
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr case_when mutate
#' @importFrom rlang .data
#'
#' @return [data.frame] with `broadClass` column.
.defineBroadClass <- function(object) {
    assert_is_data.frame(object)
    assert_is_subset(c("biotype", "symbol"), colnames(object))
    mutate(
        object,
        broadClass = case_when(
            grepl(
                x = .data[["symbol"]],
                # Hsapiens: `MT-`,
                # Mmusculus: `mt-`
                # Dmelanogaster: `mt:`
                pattern = "^mt[\\:\\-]",
                ignore.case = TRUE
            ) ~ "mito",
            .data[["biotype"]] == "protein_coding" ~ "coding",
            .data[["biotype"]] %in% c(
                "known_ncrna",
                "lincRNA",
                "non_coding"
            ) ~ "noncoding",
            grepl(
                pattern = "pseudo",
                x = .data[["biotype"]]
            ) ~ "pseudo",
            .data[["biotype"]] %in% c(
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
            .data[["biotype"]] %in% c(
                "non_stop_decay",
                "nonsense_mediated_decay"
            ) ~ "decaying",
            grepl(
                pattern = "^ig_",
                x = .data[["biotype"]],
                ignore.case = TRUE
            ) ~ "ig",
            grepl(
                pattern = "^tr_",
                x = .data[["biotype"]],
                ignore.case = TRUE
            ) ~ "tcr",
            TRUE ~ "other")
    )
}



#' Prepare Annotable
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr distinct everything group_by left_join mutate rename select
#'   summarize_all ungroup
#' @importFrom rlang !! !!! sym syms
#' @importFrom S4Vectors aggregate
#' @importFrom stats formula
#' @importFrom tibble as_tibble
#'
#' @inheritParams general
#'
#' @return [data.frame].
.annotable.data.frame <- function(object) {  # nolint
    assert_is_data.frame(object)
    assert_is_subset(annotableCols, colnames(object))

    # Inform the user if NA gene rows are present
    if (has_rownames(object)) {
        if (!identical(rownames(object), object[["ensgene"]])) {
            setdiff <- setdiff(rownames(object), object[["ensgene"]])
            warn(paste(
                "Genes without annotations:", toString(sort(setdiff))
            ))
            object[["ensgene"]] <- rownames(object)
        }
    }
    assert_all_are_not_na(object[["ensgene"]])

    # Now safe to coerce to tibble
    object <- as_tibble(object)

    # Fix any `AsIs` columns resulting from DataFrame to data frame coercion
    is.AsIs <- function(x) is(x, "AsIs")  # nolint
    asIsCol <- any(vapply(
        X = object,
        FUN = is.AsIs,
        FUN.VALUE = logical(1L),
        USE.NAMES = TRUE
    ))
    if (isTRUE(asIsCol)) {
        as.list <- function(x) as(x, "list")  # nolint
        object <- mutate_if(object, is.AsIs, as.list)
        rm(as.list)
    }
    rm(is.AsIs)

    # Check for Entrez identifier column rename to `entrez`.
    # ensembldb outputs as `entrezid`.
    entrezCol <- colnames(object) %>%
        .[grepl(x = ., pattern = "entrez")]
    if (length(entrezCol) && entrezCol != "entrez") {
        assert_is_a_string(entrezCol)
        object <- rename(object, entrez = !!sym(entrezCol))
    }

    # Collapse (nest) Entrez identifiers from long format, if necessary
    if (
        any(duplicated(object[["ensgene"]])) &&
        !is.null(object[["entrez"]]) &&
        !is.list(object[["entrez"]])
    ) {
        inform("Nesting Entrez identifiers")
        # Alternatively can use `tidyr::nest()` approach here instead but
        # the output structure won't be consistent with the ensembl return.
        entrez <- aggregate(
            formula = formula("entrez~ensgene"),
            data = object,
            FUN = list
        )
        # Now drop the `entrez` column and add the aggregated list version
        object <- object %>%
            mutate(entrez = NULL) %>%
            distinct() %>%
            left_join(entrez, by = "ensgene")
    }

    assert_has_no_duplicates(object[["ensgene"]])

    object %>%
        camel() %>%
        fixNA() %>%
        .defineBroadClass() %>%
        select(c(annotableCols, "broadClass"), everything()) %>%
        arrange(!!sym("ensgene")) %>%
        as.data.frame() %>%
        set_rownames(.[["ensgene"]])
}



# Methods ======================================================================
#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("character"),
    .annotable.character)



#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("data.frame"),
    .annotable.data.frame)
