# Create a minimal example of RefSeq GRCh38 GTF file.

setwd("data-raw")
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
    destfile = file.path("refseq_GRCh38.gff.gz")
)
gunzip("refseq_GRCh38.gff.gz", remove = FALSE, overwrite = TRUE)
system(command = "head -n 50 refseq_GRCh38.gff > refseq_GRCh38_head.gff")
file.rename("refseq_GRCh38_head.gff", "refseq_GRCh38.gff")
gzip("refseq_GRCh38.gff", overwrite = TRUE)
setwd("..")
