# nolint start
#
# *Mus musculus* PANTHER annotations
# Michael Steinbaugh
# 2018-04-07
#
# Latest version of this script is available here:
# script <- system.file(
#     file.path("scripts", "panther", "mmusculus.R"),
#     package = "basejump"
# )
# file.edit(script)
#
# nolint end

library(basejump)
library(magrittr)
library(tidyverse)

# MGI annotations ====
mgi <- read_tsv(
    file = paste(
        "http://www.informatics.jax.org",
        "downloads",
        "reports",
        "MGI_Gene_Model_Coord.rpt",
        sep = "/"
    ),
    na = c("", "NA", "null"),
    col_names = FALSE,
    skip = 1L
) %>%
    .[, c(1L, 11L)] %>%
    set_colnames(c("mgi", "geneID")) %>%
    mutate(mgi = str_replace(mgi, "^MGI\\:", ""))
saveData(mgi)

# PANTHER annotations ====
pantherFile <- transmit(
    remoteDir = paste(
        "ftp://ftp.pantherdb.org",
        "sequence_classifications",
        "current_release",
        "PANTHER_Sequence_Classification_files",
        sep = "/"
    ),
    pattern = "mouse",
    compress = TRUE,
    localDir = "annotations"
)
panther <- read_tsv(
    file = pantherFile,
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
panther <- select(panther, -c(protein, subfamily))

# Add panther prefix to all columns
colnames(panther) <- camel(paste("panther", colnames(panther), sep = "."))

# Match by Ensembl ID and combine ====
# First extract the annotations that match Ensembl
ensemblMatched <- panther %>%
    # First extract the Ensembl ID
    mutate(
        geneID = str_extract(pantherID, "Ensembl=ENSMUSG[0-9]{11}"),
        geneID = str_replace(geneID, "^Ensembl=", "")
    ) %>%
    filter(!is.na(geneID))

# Most of the annotations are mapped to HGNC for human
mgiMatched <- panther %>%
    filter(!pantherID %in% ensemblMatched[["pantherID"]]) %>%
    # Extract the HGNC ID, which we will use for join with HGNC annotations
    # to match the Ensembl ID.
    mutate(
        mgi = str_extract(pantherID, "MGI=[0-9]+"),
        mgi = str_replace(mgi, "^MGI=", "")
    ) %>%
    filter(!is.na(mgi)) %>%
    left_join(mgi, by = "mgi") %>%
    filter(!is.na(geneID)) %>%
    select(-mgi)

# Now combine PANTHER annotations that are matched to Ensembl ID
pantherWithEnsembl <- bind_rows(ensemblMatched, mgiMatched) %>%
    select(-pantherID) %>%
    select(geneID, everything()) %>%
    distinct() %>%
    arrange(geneID) %>%
    group_by(geneID) %>%
    top_n(n = 1L, wt = pantherSubfamilyName) %>%
    ungroup() %>%
    as.data.frame() %>%
    set_rownames(.[["geneID"]])
saveData(pantherWithEnsembl)
