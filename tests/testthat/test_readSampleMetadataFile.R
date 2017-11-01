context("readSampleMetadataFile")

test_that("Demultiplexed FASTQ", {
    file <- file.path(
        "http://basejump.seq.cloud",
        "sample_metadata",
        "demultiplexed.xlsx"
    )
    meta <- readSampleMetadataFile(file, quiet = TRUE)

    # Check that names are sanitized correctly
    rows <- c("sample_1",
              "sample_2",
              "sample_3",
              "sample_4")
    expect_equal(
        meta[["sampleID"]],
        rows
    )
    expect_equal(
        rownames(meta),
        rows
    )

    # Check that column names get set correctly
    expect_equal(
        colnames(meta),
        c("sampleID",
          "sampleName",
          "description",
          "fileName",
          "genotype")
    )

    # Lane-split technical replicate support
    meta <- readSampleMetadataFile(file, lanes = 4, quiet = TRUE)
    expect_equal(
        rownames(meta)[1L:8L],
        c("sample_1_L001",
          "sample_1_L002",
          "sample_1_L003",
          "sample_1_L004",
          "sample_2_L001",
          "sample_2_L002",
          "sample_2_L003",
          "sample_2_L004")
    )
    expect_equal(
        meta[1, metadataPriorityCols],
        data.frame(
            sampleID = "sample_1_L001",
            sampleName = "sample 1_L001",
            description = "sample 1_L001",
            row.names = "sample_1_L001")
    )

    # Error on file containing redundant `description` and `sampleName` columns
    expect_error(
        readSampleMetadataFile(
            file.path(
                "http://basejump.seq.cloud",
                "sample_metadata",
                "demultiplexed_with_sampleName.csv"
            ),
            quiet = TRUE
        ),
        paste("Specify only 'description' and omit 'sampleName'",
              "for demultiplexed FASTQ file metadata")
    )

    # Required column check failure
    expect_error(
        readSampleMetadataFile(
            file.path(
                "http://basejump.seq.cloud",
                "sample_metadata",
                "demultiplexed_missing_cols.csv"
            ),
            quiet = TRUE
        ),
        "Required columns: fileName, description"
    )

    # Duplicated description
    expect_error(
        readSampleMetadataFile(
            file.path("http://basejump.seq.cloud",
                      "sample_metadata",
                      "demultiplexed_duplicated_description.csv"
            ),
            quiet = TRUE
        ),
        "'description' column must be unique for demultiplexed files"
    )
})

test_that("Multiplexed FASTQ", {
    file <- file.path(
        "http://basejump.seq.cloud",
        "sample_metadata",
        "multiplexed.xlsx"
    )
    meta <- readSampleMetadataFile(file, quiet = TRUE)

    expect_equal(
        rownames(meta),
        c("run_1_CAGTTATG",
          "run_1_TTACCTCC",
          "run_2_ATAGCCTT",
          "run_2_CTTAATAG",
          "run_2_TAAGGCTC",
          "run_2_TCGCATAA",
          "run_2_TCTTACGC")
    )

    # Lane-split technical replicate support
    meta <- readSampleMetadataFile(file, lanes = 4, quiet = TRUE)
    expect_equal(
        rownames(meta),
        c("run_1_L001_CAGTTATG",
          "run_1_L001_TTACCTCC",
          "run_1_L002_CAGTTATG",
          "run_1_L002_TTACCTCC",
          "run_1_L003_CAGTTATG",
          "run_1_L003_TTACCTCC",
          "run_1_L004_CAGTTATG",
          "run_1_L004_TTACCTCC",
          "run_2_L001_ATAGCCTT",
          "run_2_L001_CTTAATAG",
          "run_2_L001_TAAGGCTC",
          "run_2_L001_TCGCATAA",
          "run_2_L001_TCTTACGC",
          "run_2_L002_ATAGCCTT",
          "run_2_L002_CTTAATAG",
          "run_2_L002_TAAGGCTC",
          "run_2_L002_TCGCATAA",
          "run_2_L002_TCTTACGC",
          "run_2_L003_ATAGCCTT",
          "run_2_L003_CTTAATAG",
          "run_2_L003_TAAGGCTC",
          "run_2_L003_TCGCATAA",
          "run_2_L003_TCTTACGC",
          "run_2_L004_ATAGCCTT",
          "run_2_L004_CTTAATAG",
          "run_2_L004_TAAGGCTC",
          "run_2_L004_TCGCATAA",
          "run_2_L004_TCTTACGC")
    )

    # Required column check failure
    expect_error(
        readSampleMetadataFile(
            file.path(
                "http://basejump.seq.cloud",
                "sample_metadata",
                "multiplexed_missing_cols.csv"
            ),
            quiet = TRUE
        ),
        "Required columns: fileName, description, sampleName, sequence"
    )

    # Duplicated sampleName
    expect_error(
        readSampleMetadataFile(
            file.path("http://basejump.seq.cloud",
                      "sample_metadata",
                      "multiplexed_duplicated_sampleName.csv"
            ),
            quiet = TRUE
        ),
        "'sampleName' column must be unique for multiplexed samples"
    )
})

test_that("Legacy bcbio samplename column", {
    file <- file.path(
        "http://basejump.seq.cloud",
        "sample_metadata",
        "bcbio_legacy_samplename.csv"
    )
    meta <- suppressWarnings(
        readSampleMetadataFile(file, quiet = TRUE)
    )
    expect_equal(
        meta,
        data.frame(
            sampleID = "sample_1",           # sanitized
            sampleName = "sample-1",         # matches description
            description = "sample-1",        # unmodified
            fileName = "sample-1.fastq.gz",  # renamed `samplename`
            row.names = "sample_1"           # sanitized
        )
    )
    expect_warning(
        readSampleMetadataFile(file, quiet = TRUE),
        "'samplename' is used in some bcbio examples for FASTQ file names"
    )
})
