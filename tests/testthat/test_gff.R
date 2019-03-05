levels <- c("genes", "transcripts")



# Local ========================================================================
context("Local GFF file import")

files <- c(
    ensemblGFF  = "ensembl.gff3.gz",
    ensemblGTF  = "ensembl.gtf.gz",
    flybaseGTF  = "flybase.gtf.gz",
    gencodeGFF  = "gencode.gff3.gz",
    gencodeGTF  = "gencode.gtf.gz",
    refseqGFF   = "refseq.gff.gz",
    wormbaseGTF = "wormbase.gtf.gz"
)
names <- names(files)
# Note that `system.file()` call drops names.
files <- system.file("extdata", files, package = "basejump")
names(files) <- names
rm(names)
stopifnot(all(file.exists(files)))

# > Ensembl ----
with_parameters_test_that(
    "Ensembl GFF", {
        file <- files[["ensemblGFF"]]
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(53L, 147L)
)

with_parameters_test_that(
    "Ensembl GTF", {
        file <- files[["ensemblGTF"]]
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)

# > FlyBase ----
with_parameters_test_that(
    "FlyBase GTF", {
        file <- files[["flybaseGTF"]]
        expect_warning(
            makeGRangesFromGFF(file, level = level),
            "GRanges does not contain biotype in mcols\\(\\)."
        )
        suppressWarnings(
            object <- makeGRangesFromGFF(file, level = level)
        )
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(39L, 72L)
)

# > GENCODE ----
with_parameters_test_that(
    "GENCODE GFF", {
        file <- files[["gencodeGFF"]]
        object <- makeGRangesFromGFF(file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)

with_parameters_test_that(
    "GENCODE GTF", {
        file <- files[["gencodeGTF"]]
        object <- makeGRangesFromGFF(file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)

# > WormBase ----
with_parameters_test_that(
    "WormBase GTF", {
        file <- files[["wormbaseGTF"]]
        expect_warning(
            makeGRangesFromGFF(file = file, level = level),
            "geneName"
        )
        suppressWarnings(
            object <- makeGRangesFromGFF(file = file, level = level)
        )
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(66L, 83L)
)

# > RefSeq ----
test_that("RefSeq GFF", {
    file <- files[["refseqGFF"]]

    # Genes
    expect_warning(
        makeGRangesFromGFF(file = file, level = "genes"),
        "RefSeq support is experimental."
    )
    suppressWarnings(
        object <- makeGRangesFromGFF(file = file, level = "genes")
    )
    expect_s4_class(object, "GRanges")
    expect_length(object, 62L)

    # Transcripts
    suppressWarnings(
        object <- makeGRangesFromGFF(file = file, level = "transcripts")
    )
    expect_s4_class(object, "GRangesList")
    expect_length(object, 100L)
})



# Remote =======================================================================
context("Remote GFF file import")
skip_if_not(
    condition = isTRUE(getOption("basejump.testthat.extra")),
    message = paste(
        "Too CPU intensive for CI.",
        "Enable with `options(basejump.testthat.extra = TRUE)`.",
        sep = "\n"
    )
)
# skip_on_appveyor(); skip_on_bioc(); skip_on_travis()



# Run more thorough unit tests when checking locally. These take too long to
# run on CI and can time out. The full checks on GFF3 files are particularly
# memory intensive, and should be run on a machine with at least 16 GB RAM.

# Wrapping with `localOrRemoteFile()` calls here so we don't download the
# file repeatedly from the remote server.



# > Ensembl ----
ensembl_gff3_file <- localOrRemoteFile(pasteURL(
    "ftp.ensembl.org",
    "pub",
    "release-95",
    "gff3",
    "homo_sapiens",
    "Homo_sapiens.GRCh38.95.gff3.gz",
    protocol = "ftp"
))
ensembl_gtf_file <- localOrRemoteFile(pasteURL(
    "ftp.ensembl.org",
    "pub",
    "release-95",
    "gtf",
    "homo_sapiens",
    "Homo_sapiens.GRCh38.95.gtf.gz",
    protocol = "ftp"
))
ensembl_lengths <- c(58735L, 206601L)

# Note that GenomicRanges chokes on Ensembl GFF3 file, so warnings in strict
# mode here are expected.
with_parameters_test_that(
    "Ensembl GFF3", {
        suppressWarnings(
            object <- makeGRangesFromGFF(
                file = ensembl_gff3_file,
                level = level,
                .checkAgainstTxDb = TRUE
            )
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = ensembl_lengths
)

with_parameters_test_that(
    "Ensembl GTF", {
        object <- makeGRangesFromGFF(
            file = ensembl_gtf_file,
            level = level,
            .checkAgainstTxDb = TRUE
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = ensembl_lengths
)



# > FlyBase ----
flybase_gtf_file <- localOrRemoteFile(pasteURL(
    "ftp.flybase.net",
    "releases",
    "FB2018_05",
    "dmel_r6.24",
    "gtf",
    "dmel-all-r6.24.gtf.gz",
    protocol = "ftp"
))
flybase_lengths <- c(17772L, 35307L)

# Expecting warnings about broad class and GenomicFeatures dropping transcripts.
with_parameters_test_that(
    "FlyBase GTF", {
        suppressWarnings(
            object <- makeGRangesFromGFF(
                file = flybase_gtf_file,
                level = level,
                .checkAgainstTxDb = TRUE
            )
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = flybase_lengths
)



# > GENCODE ----
# Expecting warning about PAR genes mismatch in strict mode.
gencode_gff3_file = localOrRemoteFile(pasteURL(
    "ftp.ebi.ac.uk",
    "pub",
    "databases",
    "gencode",
    "Gencode_human",
    "release_29",
    "gencode.v29.annotation.gff3.gz",
    protocol = "ftp"
))
gencode_gtf_file <- localOrRemoteFile(pasteURL(
    "ftp.ebi.ac.uk",
    "pub",
    "databases",
    "gencode",
    "Gencode_human",
    "release_29",
    "gencode.v29.annotation.gtf.gz",
    protocol = "ftp"
))
# Note that we're dropping PAR Y chromosome dupes on genes and transcripts.
gencode_lengths <- c(58676L, 206534L)

with_parameters_test_that(
    "GENCODE GFF3", {
        suppressWarnings(
            object <- makeGRangesFromGFF(
                file = gencode_gff3_file,
                level = level,
                .checkAgainstTxDb = TRUE
            )
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = gencode_lengths
)

with_parameters_test_that(
    "GENCODE GTF", {
        suppressWarnings(
            object <- makeGRangesFromGFF(
                file = gencode_gtf_file,
                level = level,
                .checkAgainstTxDb = TRUE
            )
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = gencode_lengths
)



# > RefSeq ----
# FIXME Can we return with a better identifier than the gene/transcript symbol?
# FIXME Can we get this to return as GRanges instead?
refseq_gff3_file <- localOrRemoteFile(pasteURL(
    "ftp.ncbi.nlm.nih.gov",
    "genomes",
    "refseq",
    "vertebrate_mammalian",
    "Homo_sapiens",
    "reference",
    "GCF_000001405.38_GRCh38.p12",
    "GCF_000001405.38_GRCh38.p12_genomic.gff.gz",
    protocol = "ftp"
))
refseq_lengths <- c(54613L, 0L)

# Expecting GenomicFeatures warnings in strict mode.
with_parameters_test_that(
    "RefSeq GFF3", {
        suppressWarnings(
            object <- makeGRangesFromGFF(
                file = refseq_gff3_file,
                level = level,
                .checkAgainstTxDb = TRUE
            )
        )
        expect_s4_class(object, "GRangesList")
        expect_identical(length(object), length)
    },
    level = levels,
    length = refseq_lengths
)



# > WormBase ----
wormbase_gtf_file <- localOrRemoteFile(pasteURL(
    "ftp.wormbase.org",
    "pub",
    "wormbase",
    "releases",
    "WS268",
    "species",
    "c_elegans",
    "PRJNA13758",
    "c_elegans.PRJNA13758.WS268.canonical_geneset.gtf.gz",
    protocol = "ftp"
))
wormbase_lengths <- c(47169L, 61388L)

# Expecting warnings about broad class and GenomicFeatures dropping transcripts.

# FIXME There seems to be an issue with WormBase GTF.
# GRanges does not contain `geneName` in mcols().
# GRanges does not contain `transcriptName` in mcols().
# Is this due to the merge step failing because of identifier garbage?
with_parameters_test_that(
    "Wormbase GTF", {
        suppressWarnings(
            object <- makeGRangesFromGFF(
                file = wormbase_gtf_file,
                level = level,
                .checkAgainstTxDb = TRUE
            )
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = wormbase_lengths
)
