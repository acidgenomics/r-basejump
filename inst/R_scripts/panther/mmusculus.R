# nolint start
#
# *Mus musculus* PANTHER annotations
# Michael Steinbaugh
# 2017-11-13
#
# Latest version of this script is available here:
# script <- system.file(
#     file.path("R_scripts", "panther", "mmusculus.R"),
#     package = "basejump")
# file.edit(script)
#
# nolint end

library(basejump)
library(tidyverse)

# MGI annotations ====
mgi <- file.path(
    "http://www.informatics.jax.org",
    "downloads",
    "reports",
    "MGI_Gene_Model_Coord.rpt") %>%
    read_tsv(col_names = FALSE, skip = 1L) %>%
    .[, c(1L, 11L)] %>%
    set_colnames(c("mgi", "ensgene")) %>%
    mutate(mgi = str_replace(mgi, "^MGI\\:", ""))
saveData(mgi)

# PANTHER annotations ====
organism <- "mouse"
pantherFTP <-
    file.path("ftp://ftp.pantherdb.org",
              "sequence_classifications",
              "current_release",
              "PANTHER_Sequence_Classification_files")
pantherFile <-
    transmit(pantherFTP,
             pattern = organism,
             compress = TRUE,
             localDir = "annotations")
panther <- read_tsv(
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
    mutate(ensgene = str_extract(pantherID, "Ensembl=ENSMUSG[0-9]{11}"),
           ensgene = str_replace(ensgene, "^Ensembl=", "")) %>%
    filter(!is.na(ensgene))

# Most of the annotations are mapped to HGNC for human
mgiMatched <- panther %>%
    filter(!pantherID %in% ensemblMatched[["pantherID"]]) %>%
    # Extract the HGNC ID, which we will use for join with HGNC annotations
    # to match the Ensembl ID.
    mutate(mgi = str_extract(pantherID, "MGI=[0-9]+"),
           mgi = str_replace(mgi, "^MGI=", "")) %>%
    left_join(mgi, by = "mgi") %>%
    filter(!is.na(ensgene)) %>%
    select(-mgi)

# Now combine PANTHER annotations that are matched to Ensembl ID
pantherWithEnsembl <- bind_rows(ensemblMatched, mgiMatched) %>%
    select(-pantherID) %>%
    select(ensgene, everything())
saveData(pantherWithEnsembl)
