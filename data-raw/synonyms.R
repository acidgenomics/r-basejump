devtools::load_all()
library(tidyverse)

# Support for C. elegans is poor here. Use WormBase instead.
genomes <- list(
    "drosophilaMelanogaster" = c(
        kingdom = "Invertebrates",
        species = "Drosophila_melanogaster"),
    "homoSapiens" = c(
        kingdom = "Mammalia",
        species = "Homo_sapiens"),
    "musMusculus" = c(
        kingdom = "Mammalia",
        species = "Mus_musculus")
)

synonyms <- lapply(genomes, function(genome) {
    data <- file.path(
        "ftp://ftp.ncbi.nih.gov",
        "gene",
        "DATA",
        "GENE_INFO",
        genome[["kingdom"]],
        paste0(genome[["species"]], ".gene_info.gz")
    ) %>%
        read_tsv()

    if (!is_tibble(data)) {
        stop(paste("Download failure:", genome[["species"]]))
    }

    data <- data %>%
        camel(strict = FALSE) %>%
        select(symbol, synonyms, dbXrefs) %>%
        filter(synonyms != "-", dbXrefs != "-") %>%
        mutate(synonyms = str_replace_all(synonyms, "\\|", ", ")) %>%
        arrange(symbol)

    # Extract the gene identifiers
    organism <- camel(genome[["species"]])
    if (organism == "drosophilaMelanogaster") {
        data <- data %>%
            mutate(
                ensgene = str_extract(dbXrefs, "\\bFBgn[0-9]{7}\\b")
            )
    } else if (organism %in% c("homoSapiens", "musMusculus")) {
        data <- data %>%
            mutate(
                ensgene = str_extract(dbXrefs, "\\bENS[A-Z]+[0-9]{11}\\b")
            )
    }

    data %>%
        filter(!is.na(ensgene)) %>%
        select(ensgene, symbol, synonyms) %>%
        arrange(ensgene)
})
names(synonyms) <- names(genomes)

synonyms[["date"]] <- Sys.Date()
synonyms[["sessionInfo"]] <- sessionInfo()

devtools::use_data(synonyms, overwrite = TRUE, compress = "xz")
