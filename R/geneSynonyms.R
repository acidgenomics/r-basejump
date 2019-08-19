#' Gene synonyms
#'
#' Look up gene synonyms from NCBI.
#'
#' @note Synonym support for *Caenorhabditis elegans* is poor on NCBI.
#' Use the [wormbase](https://steinbaugh.com/wormbase/) package instead.
#' @note Updated 2019-08-18.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return `SplitDataFrameList`.
#' Split by `geneID` column.
#' Handles genes with duplicate entries (e.g. ENSG00000004866).
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
    data <- as(data, "DataFrame")
    data <- camelCase(data)
    data <- data[, c("symbol", "synonyms", "dbXrefs")]
    colnames(data)[colnames(data) == "symbol"] <- "geneName"
    keep <- data[["synonyms"]] != "-"
    data <- data[keep, , drop = FALSE]
    keep <- data[["dbXrefs"]] != "-"
    data <- data[keep, , drop = FALSE]
    data[["synonyms"]] <- gsub(
        pattern = "\\|",
        replacement = ", ",
        x = data[["synonyms"]]
    )
    ## Sanitize the identifiers.
    if (identical(organism, "Drosophila melanogaster")) {
        ## This is covered in full local tests.
        ## nocov start
        pattern <- "\\bFBgn[0-9]{7}\\b"
        ## nocov end
    } else {
        pattern <- "\\bENS[A-Z]+[0-9]{11}\\b"
    }
    data[["geneID"]] <- str_extract(
        string = data[["dbXrefs"]],
        pattern = pattern
    )
    keep <- !is.na(data[["geneID"]])
    data <- data[keep, , drop = FALSE]
    data <- data[, c("geneID", "geneName", "synonyms")]
    data <- data[order(data[["geneID"]]), , drop = FALSE]
    split(data, f = data[["geneID"]])
}
