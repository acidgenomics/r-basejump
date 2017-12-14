devtools::load_all()
library(tidyverse)

# Support for C. elegans is poor here. Use WormBase instead.
genomes <- list(
    "drosophila_melanogaster" = c(
        "Invertebrates",
        "Drosophila_melanogaster"),
    "homo_sapiens" = c(
        "Mammalia",
        "Homo_sapiens"),
    "mus_musculus" = c(
        "Mammalia",
        "Mus_musculus")
)

synonyms <- lapply(seq_along(genomes), function(a) {
    data <- file.path(
        "ftp://ftp.ncbi.nih.gov",
        "gene",
        "DATA",
        "GENE_INFO",
        genomes[[a]][[1L]],
        paste0(genomes[[a]][[2L]], ".gene_info.gz")) %>%
        read_tsv()
    if (!is_tibble(data)) {
        stop(paste("Download failure:", genomes[[a]][[2L]]))
    }

    data <- data %>%
        camel(strict = FALSE) %>%
        select(symbol, synonyms, dbXrefs) %>%
        filter(synonyms != "-",
               dbXrefs != "-") %>%
        mutate(synonyms = str_replace_all(synonyms, "\\|", ", ")) %>%
        arrange(symbol)

    # Extract the gene identifiers
    organism <- names(genomes)[[a]]
    if (organism == "drosophila_melanogaster") {
        data <- data %>%
            mutate(
                ensgene = str_extract(
                    dbXrefs,
                    pattern = "\\bFBgn[0-9]{7}\\b"))
    } else if (organism %in% c("homo_sapiens", "mus_musculus")) {
        data <- data %>%
            mutate(
                ensgene = str_extract(
                    dbXrefs,
                    pattern = "\\bENS[A-Z]+[0-9]{11}\\b"))
    }

    data %>%
        filter(!is.na(ensgene)) %>%
        select(ensgene, symbol, synonyms) %>%
        arrange(ensgene)
})
names(synonyms) <- names(genomes)

devtools::use_data(synonyms, overwrite = TRUE, compress = "xz")
