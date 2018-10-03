# FIXME Add `.test` support for all supported organisms.



#' Gene Synonyms
#'
#' Look up gene synonyms from NCBI.
#'
#' @note Synonym support for *Caenorhabditis elegans* is poor on NCBI.
#' Use the wormbase package instead.
#'
#' @author Michael Steinbaugh
#' @family Annotation Functions
#' @export
#'
#' @param organism `string`. Supported organisms: *Homo sapiens*,
#'   *Mus musculus*, *Drosophila melanogaster*.
#'
#' @return `grouped_df`. Grouped by `geneID` column.
#'
#' @examples
#' x <- geneSynonyms(organism = "Homo sapiens", .test = TRUE)
#' print(x)
geneSynonyms <- function(organism, .test = FALSE) {
    stopifnot(has_internet())
    if (isTRUE(.test)) {
        organism <- "Homo sapiens"
    } else {
        organism <- match.arg(
            arg = organism,
            choices = .geneSynonymsOrganisms
        )
    }
    assert_is_a_bool(.test)

    # NCBI uses underscore for species name
    species <- gsub(" ", "_", organism)
    if (species == "Drosophila_melanogaster") {
        kingdom <- "Invertebrates"
    } else {
        kingdom <- "Mammalia"
    }

    genome <- c(kingdom = kingdom, species = species)

    if (isTRUE(.test)) {
        file <- file.path(basejumpCacheURL, "homo_sapiens.gene_info.gz")
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
