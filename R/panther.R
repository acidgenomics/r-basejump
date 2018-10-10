#' @inherit PANTHER-class
#'
#' @name panther
#' @family Annotation Functions
#'
#' @inheritParams general
#' @param organism `string`. Full Latin organism name. Currently supported
#'   organisms: *Homo sapiens*, *Mus musculus*, *Caenorhabditis elegans*,
#'   *Drosophila melanogaster*.
#' @param release `string` or `NULL`. PANTHER release version. If set `NULL`,
#'   defaults to current release. Consult the PANTHER website for a list of
#'   release versions available from the FTP server (e.g. `"13.0"`).
#'
#' @return `PANTHER`.
#'
#' @examples
#' invisible(capture.output(
#'     x <- panther("Homo sapiens", .test = TRUE)
#' ))
#' summary(x)
NULL



# Constructors =================================================================
.pantherMappings <- c(
    "Homo sapiens" = "human",
    "Mus musculus" = "mouse",
    "Caenorhabditis elegans" = "nematode_worm",
    "Drosophila melanogaster" = "fruit_fly"
)



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
        if (has_length(x)) {
            x
        } else {
            NULL
        }
    })
}



# Organism-specific data modifiers =============================================
.panther.homoSapiens <-  # nolint
    function(data, .test = FALSE) {
        hgnc2ensembl <- hgnc2ensembl(.test = .test) %>%
            as_tibble(rownames = NULL)

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
            left_join(hgnc2ensembl, by = "hgncID") %>%
            select(-!!sym("hgncID")) %>%
            filter(!is.na(!!sym("geneID"))) %>%
            unique()

        do.call(rbind, list(ensembl, hgnc))
    }



.panther.musMusculus <-  # nolint
    function(data, .test = FALSE) {
        mgi2ensembl <- mgi2ensembl(.test = .test) %>%
            as_tibble(rownames = NULL)

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
            left_join(mgi2ensembl, by = "mgiID") %>%
            select(-!!sym("mgiID")) %>%
            filter(!is.na(!!sym("geneID")))

        do.call(rbind, list(ensembl, mgi))
    }



.panther.drosophilaMelanogaster <-  # nolint
    function(data, .test = FALSE) {
        mutate(
            data,
            geneID = str_extract(!!sym("keys"), "FBgn\\d{7}$")
        )
    }



.panther.caenorhabditisElegans <-  # nolint
    function(data, .test = FALSE) {
        mutate(
            data,
            geneID = str_extract(!!sym("keys"), "WBGene\\d{8}$")
        )
    }



# Function =====================================================================
#' @rdname panther
#' @export
panther <- function(
    organism,
    release = NULL,
    .test = FALSE
) {
    stopifnot(has_internet())
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
    assert_is_a_bool(.test)

    message(paste0(
        "Downloading PANTHER annotations for ",
        organism,
        " (", release, ")..."
    ))

    if (isTRUE(.test)) {
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
        progress = FALSE
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
    fun <- get(paste("", "panther", camel(organism), sep = "."))
    assert_is_function(fun)
    data <- fun(data, .test = .test)
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

    message("Splitting and sorting the GO terms...")
    data <- data %>%
        mutate_at(
        .vars = c(
            "goMF",
            "goBP",
            "goCC",
            "pantherClass",
            "pantherPathway"
        ),
        .funs = .splitTerms
    ) %>%
        # Sort columns alphabetically.
        .[, sort(colnames(.)), drop = FALSE] %>%
        as("DataFrame") %>%
        set_rownames(.[["geneID"]])

    new("PANTHER", data)
}



#' @rdname panther
#' @usage NULL
#' @export
PANTHER <- panther
