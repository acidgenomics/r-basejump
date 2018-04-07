# nolint start
#
# *Homo sapiens* PANTHER annotations
# Michael Steinbaugh
# 2017-11-13

# Latest version of this script is available here:
# script <- system.file(
#     file.path("R_scripts", "panther", "hsapiens.R"),
#     package = "basejump")
# file.edit(script)
#
# nolint end

library(basejump)
library(magrittr)
library(tidyverse)

# HGNC annotations ====
hgncFile <- file.path(
    "ftp://ftp.ebi.ac.uk",
    "pub",
    "databases",
    "genenames",
    "new",
    "tsv",
    "hgnc_complete_set.txt")
hgnc <- read_tsv(hgncFile)
saveData(hgnc)
hgnc <- hgnc %>%
    camel() %>%
    select(hgncID, ensemblGeneID) %>%
    mutate(hgncID = str_replace(hgncID, "^HGNC\\:", ""))

# PANTHER annotations ====
organism <- "human"
pantherFTP <-
    file.path("ftp://ftp.pantherdb.org",
              "sequence_classifications",
              "current_release",
              "PANTHER_Sequence_Classification_files")
pantherFile <- transmit(
    pantherFTP,
    pattern = organism,
    compress = TRUE,
    localDir = "annotations")
panther <-  read_tsv(
    as.character(pantherFile),
    col_names = c(
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
    select(-c(protein, subfamily))

# Add panther prefix to all columns
colnames(panther) <- camel(paste("panther", colnames(panther), sep = "."))

# Match by Ensembl ID and combine ====
# First extract the annotations that match Ensembl
ensemblMatched <- panther %>%
    # First extract the Ensembl ID
    mutate(ensgene = str_extract(pantherID, "Ensembl=ENSG[0-9]{11}"),
           ensgene = str_replace(ensgene, "^Ensembl=", "")) %>%
    filter(!is.na(ensgene)) %>%
    distinct()

# Most of the annotations are mapped to HGNC for human
hgncMatched <- panther %>%
    filter(!pantherID %in% ensemblMatched[["pantherID"]]) %>%
    # Extract the HGNC ID, which we will use for join with HGNC annotations
    # to match the Ensembl ID.
    mutate(hgncID = str_extract(pantherID, "HGNC=[0-9]+"),
           hgncID = str_replace(hgncID, "^HGNC=", "")) %>%
    left_join(hgnc, by = "hgncID") %>%
    filter(!is.na(ensemblGeneID)) %>%
    select(-hgncID) %>%
    rename(ensgene = ensemblGeneID) %>%
    # Drop any duplicate Ensembl matches.
    # PANTHER matches by Ensembl ID take priority.
    # For example: ensgene = ENSG00000262481; hgnc = 49186
    filter(!ensgene %in% ensemblMatched[["ensgene"]]) %>%
    distinct()

# Now combine PANTHER annotations that are matched to Ensembl ID
pantherWithEnsembl <- bind_rows(ensemblMatched, hgncMatched) %>%
    select(-pantherID) %>%
    distinct() %>%
    select(ensgene, everything()) %>%
    as.data.frame() %>%
    set_rownames(.[["ensgene"]])
saveData(pantherWithEnsembl)
