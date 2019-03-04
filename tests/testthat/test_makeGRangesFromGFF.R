context("makeGRangesFromGFF")



test_that("Minimal example GFF import (CI enabled)", {
    message("FIXME")
})



test_that("Full GFF import (local only)", {
    # Run more thorough unit tests when checking locally.
    # These take too long to run on CI and can time out.
    skip_on_appveyor()
    skip_on_travis()
    skip_on_bioc()

    # Note that setting level to "transcripts" also runs the gene-level code.

    # Ensembl GTF --------------------------------------------------------------
    file <- pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-95",
        "gtf",
        "homo_sapiens",
        "Homo_sapiens.GRCh38.95.gtf.gz",
        protocol = "ftp"
    )

    object <- makeGRangesFromGFF(
        file = file,
        level = "genes",
        strict = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 58735L)

    object <- makeGRangesFromGFF(
        file = file,
        level = "transcripts",
        strict = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 206601L)

    # Ensembl GFF
    # Note that GenomicRanges chokes on this file, so strict mode will fail.
    object <- makeGRangesFromGFF(
        file = pasteURL(
            "ftp.ensembl.org",
            "pub",
            "release-95",
            "gff3",
            "homo_sapiens",
            "Homo_sapiens.GRCh38.95.gff3.gz",
            protocol = "ftp"
        ),
        level = "transcripts",
        strict = FALSE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 206601L)

    # GENCODE GTF
    object <- makeGRangesFromGFF(
        file = pasteURL(
            "ftp.ebi.ac.uk",
            "pub",
            "databases",
            "gencode",
            "Gencode_human",
            "release_29",
            "gencode.v29.annotation.gtf.gz",
            protocol = "ftp"
        ),
        level = "transcripts",
        strict = TRUE
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 206694L)

    # GENCODE GFF

    # FIXME Assert check failure.
    # Error in makeGRangesFromGFF(file = pasteURL("ftp.ebi.ac.uk", "pub", "databases",  :
    #   Assert failure.
    # hasNoDuplicates(mcols(gn)[["gene_id"]]) is not TRUE.
    # Cause of failure:
    # mcols(gn)[["gene_id"]] has duplicates at positions 58122, 58123, 58124, 58125, 58126, 58127, 58128, 58129, # 58130, 58131, 58132, 58133, 58134, 58135.....
    # Calls: makeGRangesFromGFF -> assert
    object <- makeGRangesFromGFF(
        file = pasteURL(
            "ftp.ebi.ac.uk",
            "pub",
            "databases",
            "gencode",
            "Gencode_human",
            "release_29",
            "gencode.v29.annotation.gff3.gz",
            protocol = "ftp"
        ),
        level = "transcripts",
        strict = TRUE
    )

    # RefSeq GFF
    object <- makeGRangesFromGFF(
        file = pasteURL(
            "ftp.ncbi.nlm.nih.gov",
            "genomes",
            "refseq",
            "vertebrate_mammalian",
            "Homo_sapiens",
            "reference",
            "GCF_000001405.38_GRCh38.p12",
            "GCF_000001405.38_GRCh38.p12_genomic.gff.gz",
            protocol = "ftp"
        )
    )
})
