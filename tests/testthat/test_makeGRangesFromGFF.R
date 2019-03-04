levels <- c("genes", "transcripts")



# Minimal ======================================================================
context("Minimal GFF/GTF file checks")

# Don't run `.checkAgainstTxDb = TRUE` here because it is intended to work only
# with complete GFF/GTF files.



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
    # Note that we're dropping PAR dupes on genes and transcripts.
    length = c(58676L, 206534L)
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
    # Note that we're dropping PAR dupes.
    length = c(58676L, 206601L)
)



# FIXME Document the gene/transcript differences between Ensembl and GENCODE.

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

with_parameters_test_that(
    "GENCODE GTF", {
        object <- makeGRangesFromGFF(
            file = gencode_gtf_file,
            level = level,
            .checkAgainstTxDb = TRUE
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    # FIXME We need to resolve the PAR genes more clearly here.
    length = c(58721L, 206694L)
)



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

# FIXME Unit test failure.
# Error: Test failed: 'GENCODE GFF3 '
# * `object` inherits from `CompressedGRangesList/GRangesList/GenomicRangesList/CompressedRangesList/GenomicRanges_OR_GRangesList/RangesList/CompressedList/GenomicRanges_OR_GenomicRangesList/List/Vector/list_OR_List/Annotated` not `GRanges`.
# * length(object) not identical to `length`.
# 1/1 mismatches
# [1] 58676 - 58721 == -45

with_parameters_test_that(
    "GENCODE GFF3", {
        object <- makeGRangesFromGFF(
            file = gencode_gff3_file,
            level = level,
            .checkAgainstTxDb = TRUE
        )
        expect_s4_class(object, "GRanges")
        expect_identical(length(object), length)
    },
    level = levels,
    # Difference in count is due to the PAR genes.
    length = c(58676L, 206694L)
)



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

with_parameters_test_that(
    "RefSeq GFF3", {
        object <- makeGRangesFromGFF(
            file = refseq_gff3_file,
            level = level,
            .checkAgainstTxDb = TRUE
        )
        expect_s4_class(object, "GRanges")

        print(length(object))
        # expect_identical(length(object), length)
    },
    level = levels,
    # FIXME
    # length = c(58721L, 206694L)
)
