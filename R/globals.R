globalVariables(".")

annotationCols <- c(
    "transcriptID",
    "transcriptName",
    "transcriptBiotype",
    "geneID",
    "geneName",
    "geneBiotype",
    "description"
)

# Note optional matching of gzip
extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"

# Ignore case
rdataExtPattern <- "\\.(rd[a|ata|s])$"

rdataError <- "R data files must contain `.rda`, `.rds`, or `.RData` extension."
