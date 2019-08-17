## FIXME Switch to base R approaches here.
## FIXME Switch from `==` to `identical()`.
## FIXME Remove magrittr pipe.



#' Gene synonyms
#'
#' Look up gene synonyms from NCBI.
#'
#' @note Synonym support for *Caenorhabditis elegans* is poor on NCBI.
#' Use the [wormbase](https://steinbaugh.com/wormbase/) package instead.
#' @note Updated 2019-07-28.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return `SplitDataFrame`.
#' Split by `geneID` column.
#'
#' @examples
#' options(acid.test = TRUE)
#' x <- geneSynonyms(organism = "Homo sapiens")
#' print(x)
geneSynonyms <- function(organism) {
    assert(hasInternet())
    organism <- match.arg(
        arg = organism,
        choices = c(
            "Homo sapiens",
            "Mus musculus",
            "Drosophila melanogaster"
        )
    )
    ## NCBI uses underscore for species name.
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
    data <- withCallingHandlers(
        expr = import(file = file, format = "tsv", colnames = TRUE),
        message = function(m) {
            if (isTRUE(grepl(pattern = "syntactic", x = m))) {
                invokeRestart("muffleMessage")
            } else {
                m
            }
        }
    )
    assert(hasLength(data))
    data <- camelCase(data)
    data <- data[, c("symbol", "synonyms", "dbXrefs")]
    colnames(data)[colnames(data) == "symbol"] <- "geneName"
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
