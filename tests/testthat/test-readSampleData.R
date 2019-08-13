context("readSampleData : Demultiplexed samples")

file <- file.path("cache", "bcbio-metadata-demultiplexed.csv")

test_that("DataFrame return", {
    expect_identical(
        object = readSampleData(file),
        expected = DataFrame(
            sampleName = factor(paste0("sample", seq_len(4L))),
            fileName = factor(paste0("sample", seq_len(4L), "_R1.fastq.gz")),
            description = factor(paste0("sample", seq_len(4L))),
            genotype = factor(rep(c("wildtype", "knockout"), times = 2L)),
            row.names = paste0("sample", seq_len(4L))
        )
    )
})

test_that("Lane-split technical replicate support", {
    object <- readSampleData(file, lanes = 4L)
    expect_true("lane" %in% colnames(object))
    expect_identical(
        object = rownames(object)[1L:8L],
        expected = c(
            paste0("sample1_L00", seq_len(4L)),
            paste0("sample2_L00", seq_len(4L))
        )
    )
})

test_that("Required column check failure", {
    file <- file.path(
        "cache",
        "bcbio-metadata-demultiplexed-invalid-missing-columns.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "description"
    )
})

test_that("Duplicated description", {
    file <- file.path(
        "cache",
        "bcbio-metadata-demultiplexed-invalid-duplicated.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "Sample data input file is malformed."
    )
})



context("readSampleData : Multiplexed samples")

file <- file.path("cache", "bcbio-metadata-multiplexed-indrops.csv")

test_that("DataFrame return", {
    ## Note that we're expecting this to sort by the rownames (`description`),
    ## and not by the `sampleName` column.
    expect_identical(
        object = readSampleData(file),
        expected = DataFrame(
            sampleName = factor(c(
                "sample1_1",
                "sample2_1",
                "sample3_1",
                "sample4_1",
                "sample1_2",
                "sample2_2",
                "sample3_2",
                "sample4_2"
            )),
            fileName = factor(c(
                rep("indrops1_R1.fastq.gz", times = 4L),
                rep("indrops2_R1.fastq.gz", times = 4L)
            )),
            ## Valid rownames (sampleIDs) are generated from this column.
            ## Note that we're sorting the sample metadata by this column.
            description = factor(c(
                "indrops1-ATAGAGAG",
                "indrops1-AGAGGATA",
                "indrops1-CTCCTTAC",
                "indrops1-TATGCAGT",
                "indrops2-ATAGAGAG",
                "indrops2-AGAGGATA",
                "indrops2-CTCCTTAC",
                "indrops2-TATGCAGT"
            )),
            index = factor(rep(seq_len(4L), times = 2L)),
            sequence = factor(
                rep(c(
                    "CTCTCTAT",
                    "TATCCTCT",
                    "GTAAGGAG",
                    "ACTGCATA"
                ), times = 2L)
            ),
            aggregate = factor(
                paste0("sample", rep(seq_len(4L), times = 2L)),
                levels = paste0("sample", seq_len(4L))
            ),
            genotype = factor(
                rep(c("wildtype", "knockout"), times = 4L),
                ## Note that the order should be alphabetical here.
                levels = c("knockout", "wildtype")
            ),
            revcomp = factor(
                rep(c(
                    "ATAGAGAG",
                    "AGAGGATA",
                    "CTCCTTAC",
                    "TATGCAGT"
                ), times = 2L)
            ),
            row.names = c(
                "indrops1_ATAGAGAG",
                "indrops1_AGAGGATA",
                "indrops1_CTCCTTAC",
                "indrops1_TATGCAGT",
                "indrops2_ATAGAGAG",
                "indrops2_AGAGGATA",
                "indrops2_CTCCTTAC",
                "indrops2_TATGCAGT"
            )
        )
    )
})

test_that("Lane-split technical replicate support", {
    object <- readSampleData(file, lanes = 4L)
    expect_identical(
        object = rownames(object),
        expected = c(
            "indrops1_L001_ATAGAGAG",
            "indrops1_L002_ATAGAGAG",
            "indrops1_L003_ATAGAGAG",
            "indrops1_L004_ATAGAGAG",
            "indrops2_L001_ATAGAGAG",
            "indrops2_L002_ATAGAGAG",
            "indrops2_L003_ATAGAGAG",
            "indrops2_L004_ATAGAGAG",
            "indrops1_L001_AGAGGATA",
            "indrops1_L002_AGAGGATA",
            "indrops1_L003_AGAGGATA",
            "indrops1_L004_AGAGGATA",
            "indrops2_L001_AGAGGATA",
            "indrops2_L002_AGAGGATA",
            "indrops2_L003_AGAGGATA",
            "indrops2_L004_AGAGGATA",
            "indrops1_L001_CTCCTTAC",
            "indrops1_L002_CTCCTTAC",
            "indrops1_L003_CTCCTTAC",
            "indrops1_L004_CTCCTTAC",
            "indrops2_L001_CTCCTTAC",
            "indrops2_L002_CTCCTTAC",
            "indrops2_L003_CTCCTTAC",
            "indrops2_L004_CTCCTTAC",
            "indrops1_L001_TATGCAGT",
            "indrops1_L002_TATGCAGT",
            "indrops1_L003_TATGCAGT",
            "indrops1_L004_TATGCAGT",
            "indrops2_L001_TATGCAGT",
            "indrops2_L002_TATGCAGT",
            "indrops2_L003_TATGCAGT",
            "indrops2_L004_TATGCAGT"
        )
    )
})

test_that("Required column check failure.", {
    file <- file.path(
        "cache",
        "bcbio-metadata-multiplexed-invalid-missing-columns.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "Sample data input file is malformed."
    )
})

test_that("Duplicate rows in `sampleName` column", {
    file <- file.path(
        "cache",
        "bcbio-metadata-multiplexed-invalid-duplicated.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "sampleName"
    )
})

## Recommend using `fileName` instead.
test_that("bcbio 'samplename' column", {
    file <- file.path(
        "cache",
        "bcbio-metadata-demultiplexed-invalid-legacy-samplename.csv"
    )
    expect_identical(
        colnames(readSampleData(file)),
        c("sampleName", "fileName", "description")
    )
})

test_that("`sampleID` column defined by user", {
    file <- file.path(
        "cache",
        "bcbio-metadata-demultiplexed-invalid-sample-id.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "sampleID"
    )
})

test_that("Missing file", {
    expect_error(
        object = readSampleData("XXX.csv"),
        regexp = "isAFile"
    )
})



context("readSampleData : Malformed input")

test_that("Metadata blacklist", {
    file <- file.path(
        "cache",
        "bcbio-metadata-invalid-column-name.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "sampleNames"
    )
})

test_that("Invalid description", {
    file <- file.path(
        "cache",
        "bcbio-metadata-invalid-description.csv"
    )
    expect_error(
        object = readSampleData(file),
        regexp = "Sample data input file is malformed."
    )
})
