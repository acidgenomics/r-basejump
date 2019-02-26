# Create minimal example GFF/GTF files.

# Note that these commands are memory hungry and don't always work well on a
# low power virtual machine.

setwd("data-raw")



# Ensembl GRCh38 GTF
download.file(
    url = pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-95",
        "gtf",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.95.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "ensembl.gtf.gz"
)
gunzip("ensembl.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 100 ensembl.gtf > ensembl_head.gtf")
file.rename("ensembl_head.gtf", "ensembl.gtf")
gzip("ensembl.gtf", overwrite = TRUE)



# Ensembl GRCh38 GFF
download.file(
    url = pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-95",
        "gff3",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.95.gff3.gz",
        protocol = "ftp"
    ),
    destfile = "ensembl.gff3.gz"
)
gunzip("ensembl.gff3.gz", remove = FALSE, overwrite = TRUE)
system("head -n 100 ensembl.gff3 > ensembl_head.gff3")
file.rename("ensembl_head.gff3", "ensembl.gff3")
gzip("ensembl.gff3", overwrite = TRUE)



# GENCODE GRCh38 GTF
download.file(
    url = pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_29",
        "gencode.v29.annotation.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "gencode.gtf.gz"
)
gunzip("gencode.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 100 gencode.gtf > gencode_head.gtf")
file.rename("gencode_head.gtf", "gencode.gtf")
gzip("gencode.gtf", overwrite = TRUE)



# GENCODE GRCh38 GFF
download.file(
    url = pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_29",
        "gencode.v29.annotation.gff3.gz",
        protocol = "ftp"
    ),
    destfile = "gencode.gff3.gz"
)
gunzip("gencode.gff3.gz", remove = FALSE, overwrite = TRUE)
system("head -n 100 gencode.gff3 > gencode_head.gff3")
file.rename("gencode_head.gff3", "gencode.gff3")
gzip("gencode.gff3", overwrite = TRUE)




# RefSeq GRCh38 GFF
download.file(
    url = pasteURL(
        "ftp.ncbi.nlm.nih.gov",
        "genomes",
        "refseq",
        "vertebrate_mammalian",
        "Homo_sapiens",
        "reference",
        "GCF_000001405.38_GRCh38.p12",
        "GCF_000001405.38_GRCh38.p12_genomic.gff.gz",
        protocol = "ftp"
    ),
    destfile = "refseq.gff.gz"
)
gunzip("refseq.gff.gz", remove = FALSE, overwrite = TRUE)
system("head -n 100 refseq.gff > refseq_head.gff")
file.rename("refseq_head.gff", "refseq.gff")
gzip("refseq.gff", overwrite = TRUE)



# UCSC GRCh38 (hg38) GENCODE GTF
# Use hgTables web portal to download.
# http://genome.ucsc.edu/cgi-bin/hgTables
gunzip("ucsc.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 100 ucsc.gtf > ucsc_head.gtf")
file.rename("ucsc_head.gtf", "ucsc.gtf")
gzip("ucsc.gtf", overwrite = TRUE)



# FlyBase GTF



# WormBase GTF



setwd("..")
