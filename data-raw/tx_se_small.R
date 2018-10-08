# Transcript-level SummarizedExperiment example
# 2018-10-08

organism <- "Homo sapiens"
release <- 92L

# Restrict to 1 MB per file.
limit <- structure(1e6, class = "object_size")

tx2gene <- makeTx2GeneFromEnsembl(organism, release = release)
print(tx2gene)

# Pick transcripts that have gene overlaps, to test our aggregate code.
transcripts <- c(
    "ENST00000494424",
    "ENST00000496771",
    "ENST00000612152",
    "ENST00000371584",
    "ENST00000371588",
    "ENST00000413082"
)
samples <- paste0("sample", seq_len(4L))
counts <- matrix(
    data = seq_len(length(transcripts) * length(samples)),
    byrow = TRUE,
    nrow = length(transcripts),
    ncol = length(samples),
    dimnames = list(transcripts, samples)
)
rowData <- tx2gene %>%
    as("DataFrame") %>%
    .[
        match(
            x = rownames(counts),
            table = .[["transcriptID"]]
        ),
        ,
        drop = FALSE
        ]
se <- SummarizedExperiment(
    assays = list(counts = counts),
    rowData = rowData,
    metadata = list(date = Sys.Date())
)

# Size checks.
vapply(
    X = coerceS4ToList(se),
    FUN = object.size,
    FUN.VALUE = numeric(1L)
)
stopifnot(object.size(se) < limit)
stopifnot(validObject(se))

tx_se_small <- se
devtools::use_data(tx_se_small, compress = "xz", overwrite = TRUE)
