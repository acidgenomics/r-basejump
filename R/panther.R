#' PANTHER Gene Ontology Annotations
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param release `character` string denoting PANTHER release version. If
#'   `NULL`, defaults to current release. Consult the PANTHER website for a list
#'   of release versions available from the FTP server (e.g. `"13.0"`).
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' x <- panther("Homo sapiens")
#' glimpse(hs)
panther <- function(
    organism = c(
        "human" = "Homo sapiens",
        "mouse" = "Mus musculus",
        "nematode_worm" = "Caenorhabditis elegans",
        "fruit_fly" = "Drosophila melanogaster"
    ),
    release = NULL
) {
    organism <- match.arg(organism)
    if (is.null(release)) {
        release <- "current_release"
    }
    assert_is_a_string(release)
    if (organism == "Homo sapiens") {
        data <- .pantherAnnotations("human", release = release)
        .panther.human(data)
    } else if (organism == "Mus musculus") {
        data <- .pantherAnnotations("mouse", release = release)
        .panther.mouse(data)
    } else if (organism == "Drosophila melanogaster") {
        data <- .pantherAnnotations("fruit_fly", release = release)
        .panther.fruit_fly(data)
    } else if (organism == "Caenorhabditis elegans") {
        data <- .pantherAnnotations("nematode_worm", release = release)
        .panther.nematode_worm(data)
    }
}



# Constructors =================================================================
.panther.human <- function(data) {  # nolint
    map <- hgnc2gene()

    # Ensembl matches
    ensembl <- data %>%
        mutate(
            geneID = str_extract(!!sym("keys"), "ENSG[0-9]{11}")
        ) %>%
        filter(!is.na(!!sym("geneID")))

    # HGNC matches
    hgnc <- data %>%
        # Extract the HGNC ID
        mutate(
            hgncID = str_match(!!sym("keys"), "HGNC=([0-9]+)")[, 2L],
            hgncID = as.integer(!!sym("hgncID"))
        ) %>%
        filter(!is.na(!!sym("hgncID"))) %>%
        left_join(map, by = "hgncID") %>%
        select(-!!sym("hgncID")) %>%
        filter(!is.na(!!sym("geneID"))) %>%
        unique()

    # Return
    bind_rows(ensembl, hgnc) %>%
        .pantherReturn()
}



.panther.mouse <- function(data) {  # nolint
    map <- mgi2gene()

    # Ensembl matches
    ensembl <- data %>%
        mutate(
            geneID = str_extract(!!sym("keys"), "ENSMUSG[0-9]{11}")
        ) %>%
        filter(!is.na(!!sym("geneID")))

    # MGI matches
    mgi <- data %>%
        mutate(
            mgiID = str_match(!!sym("keys"), "MGI=([0-9]+)")[, 2L],
            mgiID = as.integer(!!sym("mgiID"))
        ) %>%
        filter(!is.na(!!sym("mgiID"))) %>%
        left_join(map, by = "mgiID") %>%
        select(-!!sym("mgiID")) %>%
        filter(!is.na(!!sym("geneID")))

    # Return
    bind_rows(ensembl, mgi) %>%
        .pantherReturn()
}



.panther.fruit_fly <- function(data) {  # nolint
    data %>%
        mutate(
            geneID = str_extract(!!sym("keys"), "FBgn\\d{7}$")
        ) %>%
        .pantherReturn()
}



.panther.nematode_worm <- function(data) {  # nolint
     data %>%
        mutate(
            geneID = str_extract(!!sym("keys"), "WBGene\\d{8}$")
        ) %>%
        .pantherReturn()
}



.pantherAnnotations <- function(
    organism = c("human", "mouse", "fruit_fly", "nematode_worm"),
    release,
    dir = "."
) {
    organism <- match.arg(organism)
    file <- transmit(
        remoteDir = paste(
            "ftp://ftp.pantherdb.org",
            "sequence_classifications",
            release,
            "PANTHER_Sequence_Classification_files",
            sep = "/"
        ),
        pattern = organism,
        compress = TRUE,
        localDir = dir
    )
    read_tsv(
        file = file,
        col_names = c(
            "pantherID",
            "X2",
            "pantherSubfamily",
            "pantherFamilyName",
            "pantherSubfamilyName",
            "goMF",
            "goBP",
            "goCC",
            "pantherClass",
            "pantherPathway"
        )) %>%
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
}



.pantherReturn <- function(data) {
    data <- data %>%
        select(-!!sym("keys")) %>%
        select(!!sym("geneID"), everything()) %>%
        filter(!is.na(!!sym("geneID"))) %>%
        unique() %>%
        # Some organisms have duplicate annotations per gene ID
        group_by(!!sym("geneID")) %>%
        top_n(n = 1L, wt = !!sym("pantherSubfamily")) %>%
        ungroup() %>%
        arrange(!!sym("geneID"))
    assert_has_no_duplicates(data[["geneID"]])
    message("Splitting and sorting the GO terms")
    mutate_at(
        .tbl = data,
        .vars = c(
            "goMF",
            "goBP",
            "goCC",
            "pantherClass",
            "pantherPathway"
        ),
        .funs = .splitTerms
    ) %>%
        as.data.frame() %>%
        set_rownames(.[["geneID"]])
}



.splitTerms <- function(vec) {
    message(deparse(substitute(vec)))
    pblapply(vec, function(x) {
        x <- x %>%
            strsplit(split = ";") %>%
            unlist() %>%
            unique() %>%
            sort() %>%
            gsub("#([A-Z0-9:]+)", " [\\1]", .) %>%
            gsub(">", " > ", .)
        if (length(x)) {
            x
        } else {
            NULL
        }
    })
}
