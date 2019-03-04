context("makeGRangesFromGFF")



test_that("Minimal example GFF import (CI enabled)", {
    message("FIXME")
})



test_that("Full GFF import (local only)", {
    # Run more thorough unit tests when checking locally. These take too long to
    # run on CI and can time out. The full checks on GFF3 files are particularly
    # memory intensive, and should be run on a machine with at least 16 GB RAM.
    skip_on_appveyor()
    skip_on_travis()
    skip_on_bioc()

    # Ensembl GTF --------------------------------------------------------------
    # Wrapping with `localOrRemoteFile()` call here so we don't download the
    # file repeatedly from the remote server.
    file <- localOrRemoteFile(pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-95",
        "gtf",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.95.gtf.gz",
        protocol = "ftp"
    ))

    object <- makeGRangesFromGFF(
        file = file,
        level = "genes",
        .checkAgainstTxDb = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 58735L)

    object <- makeGRangesFromGFF(
        file = file,
        level = "transcripts",
        .checkAgainstTxDb = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 206601L)

    # Ensembl GFF --------------------------------------------------------------
    # Note that GenomicRanges chokes on this file, so warnings in strict mode
    # are expected.
    file <- localOrRemoteFile(pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-95",
        "gff3",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.95.gff3.gz",
        protocol = "ftp"
    ))

    suppressWarnings(
        object <- makeGRangesFromGFF(
            file = file,
            level = "genes",
            .checkAgainstTxDb = TRUE
        )
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 58735L)

    suppressWarnings(
        object <- makeGRangesFromGFF(
            file = file,
            level = "transcripts",
            .checkAgainstTxDb = TRUE
        )
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 206601L)

    # GENCODE GTF --------------------------------------------------------------
    file = localOrRemoteFile(pasteURL(
        "ftp.ebi.ac.uk",
        "pub",
        "databases",
        "gencode",
        "Gencode_human",
        "release_29",
        "gencode.v29.annotation.gtf.gz",
        protocol = "ftp"
    ))

    object <- makeGRangesFromGFF(
        file = file,
        level = "genes",
        .checkAgainstTxDb = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 58721L)

    object <- makeGRangesFromGFF(
        file = file,
        level = "transcripts",
        .checkAgainstTxDb = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 206694L)

    # GENCODE GFF --------------------------------------------------------------
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

    # RefSeq GFF ---------------------------------------------------------------
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
