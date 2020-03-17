## Create minimal example GFF/GTF files.
## Updated on 2020-03-16.
##
## Note that these commands are memory hungry and don't always work well on a
## low power virtual machine.
##
## Refer to koopa `download-gtf.sh` for URLs.
##
## Set the working directory to script path.

library(R.utils)   # 2.9.2
library(basejump)  # 0.12.4



## Ensembl GRCh38 GTF ====
download.file(
    url = pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-99",
        "gtf",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.99.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "ensembl.gtf.gz"
)
gunzip("ensembl.gtf.gz", remove = FALSE, overwrite = TRUE)
## Note that this manual cutoff can create issues with TxDb checks.
system("head -n 1000 ensembl.gtf > ensembl_head.gtf")
file.rename("ensembl_head.gtf", "ensembl.gtf")
gzip("ensembl.gtf", overwrite = TRUE)



## Ensembl GRCh38 GFF3 ====
download.file(
    url = pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-99",
        "gff3",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.99.gff3.gz",
        protocol = "ftp"
    ),
    destfile = "ensembl.gff3.gz"
)
gunzip("ensembl.gff3.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 ensembl.gff3 > ensembl_head.gff3")
file.rename("ensembl_head.gff3", "ensembl.gff3")
gzip("ensembl.gff3", overwrite = TRUE)



## GENCODE GRCh38 GTF ====
download.file(
    url = pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_33",
        "gencode.v33.annotation.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "gencode.gtf.gz"
)
gunzip("gencode.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 gencode.gtf > gencode_head.gtf")
file.rename("gencode_head.gtf", "gencode.gtf")
gzip("gencode.gtf", overwrite = TRUE)



## GENCODE GRCh38 GFF3 ====
download.file(
    url = pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_33",
        "gencode.v33.annotation.gff3.gz",
        protocol = "ftp"
    ),
    destfile = "gencode.gff3.gz"
)
gunzip("gencode.gff3.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 gencode.gff3 > gencode_head.gff3")
file.rename("gencode_head.gff3", "gencode.gff3")
gzip("gencode.gff3", overwrite = TRUE)



## RefSeq GRCh38.p13 GTF ====
download.file(
    url = pasteURL(
        "ftp.ncbi.nlm.nih.gov",
        "genomes",
        "refseq",
        "vertebrate_mammalian",
        "Homo_sapiens",
        "reference",
        "GCF_000001405.39_GRCh38.p13",
        "GCF_000001405.39_GRCh38.p13_genomic.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "refseq.gtf.gz"
)
gunzip("refseq.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 refseq.gtf > refseq_head.gtf")
file.rename("refseq_head.gtf", "refseq.gtf")
gzip("refseq.gtf", overwrite = TRUE)



## RefSeq GRCh38.p13 GFF3 ====
download.file(
    url = pasteURL(
        "ftp.ncbi.nlm.nih.gov",
        "genomes",
        "refseq",
        "vertebrate_mammalian",
        "Homo_sapiens",
        "reference",
        "GCF_000001405.39_GRCh38.p13",
        "GCF_000001405.39_GRCh38.p13_genomic.gff.gz",
        protocol = "ftp"
    ),
    destfile = "refseq.gff.gz"
)
gunzip("refseq.gff.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 refseq.gff > refseq_head.gff")
file.rename("refseq_head.gff", "refseq.gff")
gzip("refseq.gff", overwrite = TRUE)



## FlyBase GTF ====
download.file(
    url = pasteURL(
        "ftp.flybase.net",
        "releases",
        "FB2020_01",
        "dmel_r6.32",
        "gtf",
        "dmel-all-r6.32.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "flybase.gtf.gz"
)
gunzip("flybase.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 flybase.gtf > flybase_head.gtf")
file.rename("flybase_head.gtf", "flybase.gtf")
gzip("flybase.gtf", overwrite = TRUE)



## WormBase GTF ====
download.file(
    url = pasteURL(
        "ftp.wormbase.org",
        "pub",
        "wormbase",
        "releases",
        "WS275",
        "species",
        "c_elegans",
        "PRJNA13758",
        "c_elegans.PRJNA13758.WS275.canonical_geneset.gtf.gz",
        protocol = "ftp"
    ),
    destfile = "wormbase.gtf.gz"
)
gunzip("wormbase.gtf.gz", remove = FALSE, overwrite = TRUE)
system("head -n 1000 wormbase.gtf > wormbase_head.gtf")
file.rename("wormbase_head.gtf", "wormbase.gtf")
gzip("wormbase.gtf", overwrite = TRUE)
