library(basejump)
library(stringr)
library(readr)
library(tidyverse)



# HGNC annotations ====
hgncTXT <-
    file.path("ftp://ftp.ebi.ac.uk",
              "pub",
              "databases",
              "genenames",
              "new",
              "tsv",
              "hgnc_complete_set.txt")
hgnc <- read_tsv(hgncTXT)
saveData(hgnc)

hgnc <- hgnc %>%
    dplyr::select(hgnc_id, ensembl_gene_id) %>%
    mutate(hgnc_id = str_replace(hgnc_id, "^HGNC\\:", ""))



# PANTHER annotations ====
pantherFTP <-
    file.path("ftp://ftp.pantherdb.org",
              "sequence_classifications",
              "current_release",
              "PANTHER_Sequence_Classification_files")
pantherFile <-
    transmit(pantherFTP,
             pattern = "human",
             compress = TRUE,
             localDir = "annotations")
panther <- pantherFile %>%
    as.character %>%
    read_tsv(col_names = c(
        "id",
        "protein",
        "subfamily",
        "familyName",
        "subfamilyName",
        "geneOntologyMolecularFunction",
        "geneOntologyBiologicalProcess",
        "geneOntologyCellularComponent",
        "class",
        "pathway"
    ))
saveData(panther)

# Remove unnecessary columns
panther <- panther %>%
    dplyr::select(-c(protein, subfamily))

# Add panther prefix to all columns
colnames(panther) <- camel(paste("panther", colnames(panther), sep = "."))



# Match by Ensembl ID and combine ====
# First extract the annotations that match Ensembl
ensemblMatched <- panther %>%
    # First extract the Ensembl ID
    mutate(ensgene = str_extract(pantherId, "Ensembl=ENSG[0-9]{11}"),
           ensgene = str_replace(ensgene, "^Ensembl=", "")) %>%
    dplyr::filter(!is.na(ensgene))

# Most of the annotations are mapped to HGNC for human
hgncMatched <- panther %>%
    dplyr::filter(!pantherId %in% ensemblMatched[["pantherId"]]) %>%
    # Extract the HGNC ID, which we will use for join with HGNC annotations
    # to match the Ensembl ID.
    mutate(hgnc_id = str_extract(pantherId, "HGNC=[0-9]+"),
           hgnc_id = str_replace(hgnc_id, "^HGNC=", "")) %>%
    left_join(hgnc, by = "hgnc_id") %>%
    dplyr::filter(!is.na(ensembl_gene_id)) %>%
    select(-hgnc_id) %>%
    rename(ensgene = ensembl_gene_id)

# Now combine PANTHER annotations that are matched to Ensembl ID
pantherWithEnsembl <- bind_rows(ensemblMatched, hgncMatched) %>%
    dplyr::select(-pantherId) %>%
    dplyr::select(ensgene, everything())
saveData(pantherWithEnsembl)
