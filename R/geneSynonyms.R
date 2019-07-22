#' Gene synonyms
#'
#' Look up gene synonyms from NCBI.
#'
#' @note Synonym support for *Caenorhabditis elegans* is poor on NCBI.
#' Use the [wormbase](https://steinbaugh.com/wormbase/) package instead.
#'
#' @export
#'
#' @inheritParams params
#' @param organism `character(1)`.
#'   Supported organisms:
#'
#'   - *Homo sapiens*.
#'   - *Mus musculus*.
#'   - *Drosophila melanogaster*.
#'
#' @return `grouped_df`.
#' Grouped by `geneID` column.
#'
#' @examples
#' options(acid.test = TRUE)
#' x <- geneSynonyms(organism = "Homo sapiens")
#' print(x)
geneSynonyms <- function(organism) {
    assert(hasInternet())
    organism <- match.arg(arg = organism, choices = .geneSynonymsOrganisms)

    ## NCBI uses underscore for species name
    species <- gsub(" ", "_", organism)
    if (species == "Drosophila_melanogaster") {
        ## This is covered in full local tests.
        kingdom <- "Invertebrates"  # nocov
    } else {
        kingdom <- "Mammalia"
    }

    genome <- c(kingdom = kingdom, species = species)

    if (isTRUE(getOption("acid.test"))) {
        assert(organism == "Homo sapiens")
        file <- pasteURL(
            basejumpTestsURL, paste0(snake(organism), ".gene_info.gz"),
            protocol = "none"
        )
    } else {
        ## This works unreliably on Travis, so cover locally instead.
        ## nocov start
        file <- pasteURL(
            "ftp.ncbi.nih.gov",
            "gene",
            "DATA",
            "GENE_INFO",
            genome[["kingdom"]],
            paste0(genome[["species"]], ".gene_info.gz"),
            protocol = "ftp"
        )
        ## nocov end
    }

    data <- read_tsv(file = file, col_types = cols(), progress = FALSE)
    assert(hasLength(data))

    data <- data %>%
        camel() %>%
        select(!!!syms(c("symbol", "synonyms", "dbXrefs"))) %>%
        rename(geneName = !!sym("symbol")) %>%
        filter(
            !!sym("synonyms") != "-",
            !!sym("dbXrefs") != "-"
        ) %>%
        mutate(synonyms = str_replace_all(!!sym("synonyms"), "\\|", ", "))

    ## Sanitize the identifiers.
    if (organism == "Drosophila melanogaster") {
        ## This is covered in full local tests.
        ## nocov start
        data <- mutate(
            data,
            geneID = str_extract(!!sym("dbXrefs"), "\\bFBgn[0-9]{7}\\b")
        )
        ## nocov end
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



## Using this for parameterized unit testing.
.geneSynonymsOrganisms <- c(
    "Homo sapiens",
    "Mus musculus",
    "Drosophila melanogaster"
)
