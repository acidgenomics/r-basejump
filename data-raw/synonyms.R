library(stringr)
library(tidyverse)

# Support for C. elegans is poor here. Use WormBase instead.
genomes <- list(
    dmelanogaster = c("Invertebrates", "Drosophila_melanogaster"),
    hsapiens = c("Mammalia", "Homo_sapiens"),
    mmusculus = c("Mammalia", "Mus_musculus"))

synonyms <- lapply(seq_along(genomes), function(a) {
    tbl <- file.path(
        "ftp://ftp.ncbi.nih.gov",
        "gene",
        "DATA",
        "GENE_INFO",
        genomes[[a]][[1L]],
        paste0(genomes[[a]][[2L]], ".gene_info.gz")) %>%
        read_tsv() %>%
        camel(strict = FALSE) %>%
        select(symbol, synonyms, dbXrefs) %>%
        filter(synonyms != "-",
               dbXrefs != "-") %>%
        mutate(synonyms = str_replace_all(synonyms, "\\|", ", ")) %>%
        arrange(symbol)

    # Extract the gene identifiers
    organism <- names(genomes)[[a]]
    if (organism == "dmelanogaster") {
        tbl <- tbl %>%
            mutate(
                ensgene = str_extract(
                    dbXrefs,
                    pattern = "\\bFBgn[0-9]{7}\\b"))
    } else if (organism %in% c("hsapiens", "mmusculus")) {
        tbl <- tbl %>%
            mutate(
                ensgene = str_extract(
                    dbXrefs,
                    pattern = "\\bENS[A-Z]+[0-9]{11}\\b"))
    }

    tbl %>%
        filter(!is.na(ensgene)) %>%
        select(ensgene, symbol, synonyms) %>%
        arrange(ensgene)
}) %>%
    set_names(names(genomes))

save(synonyms,
     file = "~/Desktop/synonyms.rda",
     compress = "xz")
