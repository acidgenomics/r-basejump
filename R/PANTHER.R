#' @name PANTHER
#' @inherit PANTHER-class
#' @inheritParams params
#'
#' @param organism `string`. Full Latin organism name. Currently supported
#'   organisms: *Homo sapiens*, *Mus musculus*, *Caenorhabditis elegans*,
#'   *Drosophila melanogaster*.
#' @param release `string` or `NULL`. PANTHER release version. If set `NULL`,
#'   defaults to current release. Consult the PANTHER website for a list of
#'   release versions available from the FTP server (e.g. `"13.0"`).
#' @param progress `boolean`. Call `pbapply::pblapply()` internally to show
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



.splitTerms <- function(x, progress = FALSE) {
    if (isTRUE(progress)) {
        message(deparse(substitute(x)))
        lapply <- pblapply
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
    assertHasInternet()
    organism <- match.arg(
        arg = organism,
        choices = names(.pantherMappings)
    )
    pantherName <-  .pantherMappings[[organism]]
    assertString(pantherName)
    if (is.null(release)) {
        release <- "current_release"
    }
    assertString(release)
    assertFlag(progress)

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
    assertString(file)

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
    assertHasRows(data)

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
