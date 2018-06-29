# Synonym support for C. elegans is poor on NCBI.
# Refer users to `wormbase` package instead.

library(devtools)
library(tidyverse)
load_all()

genomes <- list(
    homoSapiens = c(
        kingdom = "Mammalia",
        species = "Homo_sapiens"
    ),
    musMusculus = c(
        kingdom = "Mammalia",
        species = "Mus_musculus"
    ),
    drosophilaMelanogaster = c(
        kingdom = "Invertebrates",
        species = "Drosophila_melanogaster"
    )
)

synonyms <- lapply(genomes, function(genome) {
    data <- paste(
        "ftp://ftp.ncbi.nih.gov",
        "gene",
        "DATA",
        "GENE_INFO",
        genome[["kingdom"]],
        paste0(genome[["species"]], ".gene_info.gz"),
        sep = "/"
    ) %>%
        read_tsv()

    if (!is_tibble(data)) {
        stop(paste("Download failure:", genome[["species"]]))
    }

    data <- data %>%
        camel() %>%
        select(symbol, synonyms, dbXrefs) %>%
        rename(geneName = symbol) %>%
        filter(
            synonyms != "-",
            dbXrefs != "-"
        ) %>%
        mutate(synonyms = str_replace_all(synonyms, "\\|", ", "))

    # Extract the gene identifiers
    organism <- camel(genome[["species"]])
    if (organism == "drosophilaMelanogaster") {
        data <- mutate(
            data,
            geneID = str_extract(dbXrefs, "\\bFBgn[0-9]{7}\\b")
        )
    } else if (organism %in% c("homoSapiens", "musMusculus")) {
        data <- mutate(
            data,
            geneID = str_extract(dbXrefs, "\\bENS[A-Z]+[0-9]{11}\\b")
        )
    }

    data %>%
        filter(!is.na(geneID)) %>%
        select(geneID, geneName, synonyms) %>%
        arrange(geneID)
})
names(synonyms) <- names(genomes)

synonyms[["date"]] <- Sys.Date()
use_data(synonyms, overwrite = TRUE, compress = "xz")
