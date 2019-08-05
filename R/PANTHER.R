#' @rdname PANTHER-class
#' @export
#'
#' @inheritParams acidroxygen::params
#' @param organism `character(1)`.
#'   Full Latin organism name.
#'
#'   Supported organisms:
#'
#'   - *Caenorhabditis elegans*
#'   - *Drosophila melanogaster*
#'   - *Homo sapiens*
#'   - *Mus musculus*
#' @param release `character(1)` or `NULL`.
#'   PANTHER release version. If `NULL`, defaults to current release. Consult
#'   the PANTHER website for a list of release versions available from the FTP
#'   server (e.g. `"14.0"`).
#' @param progress `logical(1)`.
#'   Use `pbapply::pblapply()` to show progress.
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

    message(paste0(
        "Downloading PANTHER annotations for ",
        organism,
        " (", release, ")."
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
