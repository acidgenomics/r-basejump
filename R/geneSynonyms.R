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
#' @param organism `string`. Supported organisms:
#'   - *Homo sapiens*
#'   - *Mus musculus*
#'   - *Drosophila melanogaster*
#'
#' @return `grouped_df`. Grouped by `geneID` column.
#'
#' @examples
#' x <- geneSynonyms(organism = "Homo sapiens")
#' print(x)
geneSynonyms <- function(organism) {
    stopifnot(has_internet())
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

    data <- read_tsv(
        file = paste(
            "ftp://ftp.ncbi.nih.gov",
            "gene",
            "DATA",
            "GENE_INFO",
            genome[["kingdom"]],
            paste0(genome[["species"]], ".gene_info.gz"),
            sep = "/"
        )
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



.geneSynonymsOrganisms <- c(
    "Homo sapiens",
    "Mus musculus",
    "Drosophila melanogaster"
)
