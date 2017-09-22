library(basejump)
library(stringr)
library(readr)
library(tidyverse)

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

# Remove unnecessary columns
panther <- panther %>%
    dplyr::select(-c(protein, subfamily))
saveData(panther)

# Mouse uses MGI annotations -- need to add this
