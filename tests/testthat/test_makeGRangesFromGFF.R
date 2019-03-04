levels <- c("genes", "transcripts")



# Minimal ======================================================================
context("Minimal GFF/GTF file checks")



# CPU-intensive ================================================================
context("Remote GFF/GTF file checks (local only)")

# Run more thorough unit tests when checking locally. These take too long to
# run on CI and can time out. The full checks on GFF3 files are particularly
# memory intensive, and should be run on a machine with at least 16 GB RAM.

# Wrapping with `localOrRemoteFile()` calls here so we don't download the
# file repeatedly from the remote server.

skip_on_appveyor()
skip_on_bioc()
skip_on_travis()



ensembl_gtf_file <- localOrRemoteFile(pasteURL(
    "ftp.ensembl.org",
    "pub",
    "release-95",
    "gtf",
    "homo_sapiens",
    "Homo_sapiens.GRCh38.95.gtf.gz",
    protocol = "ftp"
))

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
    length = c(58735L, 206601L)
)



ensembl_gff3_file <- localOrRemoteFile(pasteURL(
    "ftp.ensembl.org",
    "pub",
    "release-95",
    "gff3",
    "homo_sapiens",
    "Homo_sapiens.GRCh38.95.gff3.gz",
    protocol = "ftp"
))

# Note that GenomicRanges chokes on Ensembl GFF3 file, so warnings in strict
# mode here are expected.
with_parameters_test_that(
    "Ensembl GFF3", {
        # FIXME The warning suppression method using `muffleRestart` breaks down here.
        # FIXME Assert check error:
        # assert(isSubset(names(gr2), names(gr1)))
        # .checkGRangesAgainstTxDb makeGRangesFromGFF.R:260
        # assert(isSubset(names(gr2), names(gr1)))
        # makeGRangesFromGFF.R:949
        object <- makeGRangesFromGFF(
            file = ensembl_gff3_file,
            level = level,
            .checkAgainstTxDb = TRUE
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    # This should match GTF (see above).
    length = c(58735L, 206601L)
)



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

# FIXME What are the gene/transcript differences between Ensembl and GENCODE?
with_parameters_test_that(
    "GENCODE GTF", {
        object <- makeGRangesFromGFF(
            file = ensembl_gtf_file,
            level = level,
            .checkAgainstTxDb = TRUE
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    length = c(58721L, 206694L)
)



test_that("GENCODE GFF3", {
    file = localOrRemoteFile(pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_29",
        "gencode.v29.annotation.gff3.gz",
        protocol = "ftp"
    ))

    # GRanges or GRangesList?
    # FIXME This step is breaking
    # Error in .checkGRangesAgainstTxDb(gr = out, txdb = txdb) :
    # Assert failure.
    # hasNames(gr) is not TRUE.
    # Cause of failure:
    #     The names of gr are NULL.
    object <- makeGRangesFromGFF(
        file = file,
        level = "genes",
        .checkAgainstTxDb = TRUE
    )

    object <- makeGRangesFromGFF(
        file = file,
        level = "transcripts",
        .checkAgainstTxDb = TRUE
    )
})



test_that("RefSeq GFF3", {
    file <- localOrRemoteFile(pasteURL(
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

    # GRanges or GRangesList?
    object <- makeGRangesFromGFF(
        file = file,
        level = "genes",
        .checkAgainstTxDb = TRUE
    )

    object <- makeGRangesFromGFF(
        file = file,
        level = "transcripts",
        .checkAgainstTxDb = TRUE
    )
})
