## Updated 2020-01-19.

localOrRemoteFile <- pipette::localOrRemoteFile
pasteURL <- acidbase::pasteURL

## Enable this for even more verbose checks.
checkAgainstTxDb <- TRUE

levels <- c("genes", "transcripts")

files <- list(
    ensemblGFF3 = pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-97",
        "gff3",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.97.gff3.gz",
        protocol = "ftp"
    ),
    ensemblGTF = pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-97",
        "gtf",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.97.gtf.gz",
        protocol = "ftp"
    ),
    flybaseGTF = pasteURL(
        "ftp.flybase.net",
        "releases",
        "FB2019_04",
        "dmel_r6.29",
        "gtf",
        "dmel-all-r6.29.gtf.gz",
        protocol = "ftp"
    ),
    gencodeGFF3 = pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_31",
        "gencode.v31.annotation.gff3.gz",
        protocol = "ftp"
    ),
    gencodeGTF = pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_31",
        "gencode.v31.annotation.gtf.gz",
        protocol = "ftp"
    ),
    refseqGFF3 = pasteURL(
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
    refseqGTF = pasteURL(
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
    wormbaseGTF = pasteURL(
        "ftp.wormbase.org",
        "pub",
        "wormbase",
        "releases",
        "WS273",
        "species",
        "c_elegans",
        "PRJNA13758",
        "c_elegans.PRJNA13758.WS273.canonical_geneset.gtf.gz",
        protocol = "ftp"
    )
)



## Ensembl =====================================================================
context("Ensembl")

## GenomicRanges chokes on Ensembl GFF3 file, so warnings are expected.

skip_if_not(hasInternet(url = "ftp://ftp.ensembl.org/"))
lengths <- c(genes = 60617L, transcripts = 226788L)

file <- localOrRemoteFile(files[["ensemblGTF"]])
with_parameters_test_that(
    "GTF", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = checkAgainstTxDb
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)

file <- localOrRemoteFile(files[["ensemblGFF3"]])
with_parameters_test_that(
    "GFF3", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = checkAgainstTxDb
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = ensemblLengths
)



## FlyBase =====================================================================
context("FlyBase")

## Expecting warnings about broad class and dropped transcripts.

skip_if_not(hasInternet(url = "ftp://ftp.flybase.net/"))
lengths <- c(genes = 17825L, transcripts = 35421L)

file <- localOrRemoteFile(files[["flybaseGTF"]])
with_parameters_test_that(
    "GTF", {
        suppressWarnings({
            object <- makeGRangesFromGFF(
                file = file,
                level = level,
                .checkAgainstTxDb = checkAgainstTxDb
            )
        })
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)



## GENCODE =====================================================================
context("GENCODE")

## Expecting warning about PAR genes mismatch in strict mode.
## Note that we're dropping PAR Y chromosome dupes on genes and transcripts.
## 45 PAR dupe genes; 161 PAR dupe transcripts.

skip_if_not(hasInternet(url = "ftp://ftp.ebi.ac.uk/"))
lengths <- c(genes = 60603L, transcripts = 226882L)

file <- localOrRemoteFile(files[["gencodeGTF"]])
with_parameters_test_that(
    "GTF", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = checkAgainstTxDb
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)

file <- localOrRemoteFile(files[["gencodeGFF3"]])
with_parameters_test_that(
    "GFF3", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = checkAgainstTxDb
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)



## RefSeq ======================================================================
context("RefSeq")

# GenomicFeatures currently chokes on RefSeq files.

skip_if_not(hasInternet(url = "ftp://ftp.ncbi.nlm.nih.gov/"))

file <- localOrRemoteFile(files[["refseqGTF"]])
lengths <- c(genes = 61024L, transcripts = 171642L)
with_parameters_test_that(
    "GTF", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = FALSE
        )
        expect_s4_class(object, "GRangesList")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)

file <- localOrRemoteFile(files[["refseqGFF3"]])
lengths <- c(genes = 54534L, transcripts = 158792L)
with_parameters_test_that(
    "GFF3", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = FALSE
        )
        expect_s4_class(object, "GRangesList")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)



## WormBase ====================================================================
context("WormBase")

skip_if_not(hasInternet(url = "ftp://ftp.wormbase.org/"))
lengths <- c(46911L, 72718L)

file <- localOrRemoteFile(files[["wormbaseGTF"]])
with_parameters_test_that(
    "GTF", {
        object <- makeGRangesFromGFF(
            file = file,
            level = level,
            .checkAgainstTxDb = checkAgainstTxDb
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = lengths
)
