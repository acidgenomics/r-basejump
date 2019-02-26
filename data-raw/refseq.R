download.file(
    url = paste(
        "ftp://ftp.ncbi.nlm.nih.gov",
        "genomes",
        "refseq",
        "vertebrate_mammalian",
        "Homo_sapiens",
        "reference",
        "GCF_000001405.38_GRCh38.p12",
        "GCF_000001405.38_GRCh38.p12_genomic.gff.gz",
        sep = "/"
    ),
    destfile = file.path("data-raw/refseq-GRCh38.gff.gz")
)

# Decompress, subset the head, recompress, and save to S3 bucket.
# This is pretty slow on the entire file...takes a few minutes.
# Is this due to a memory crash?
# Error: Error occurred during transmission
refseq <- rtracklayer::import("data-raw/refseq-GRCh38.gff.gz")
