context("sampleYAML")

yaml <- readYAML(
    file.path(
        "http://basejump.seq.cloud",
        "bcbio",
        "project-summary.yaml"),
    quiet = TRUE)

test_that("sampleYAML", {
    expect_equal(
        sampleYAML(yaml, "metadata"),
        tibble(
            group = c(
                "ctrl",
                "ctrl",
                "ko",
                "ko"),
            description = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2")
        )
    )
})

test_that("Invalid YAML list", {
    empty <- list()
    expect_error(
        sampleYAML(empty, keys = "metadata")
    )
    # Fix this method in a future update
    expect_equal(
        sampleYAML(yaml, keys = "XXX"),
        NULL
    )
    expect_equal(
        sampleYAML(yaml, keys = c("XXX", "YYY")),
        NULL
    )
})

test_that("sampleYAMLMetadata", {
    expect_equal(
        sampleYAMLMetadata(yaml),
        data.frame(
            sampleID = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2"),
            sampleName = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2"),
            description = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2"),
            group = as.factor(c(
                "ctrl",
                "ctrl",
                "ko",
                "ko")),
            row.names = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2")
        )
    )
})

test_that("sampleYAMLMetrics", {
    metrics <- sampleYAMLMetrics(yaml)
    # Column names
    expect_equal(
        colnames(metrics),
        c("sampleID",
          "sampleName",
          "description",
          "xGC",
          "x53Bias",
          "averageInsertSize",
          "duplicates",
          "duplicationRateOfMapped",
          "exonicRate",
          "intergenicRate",
          "intronicRate",
          "mappedPairedReads",
          "mappedReads",
          "name",
          "qualityFormat",
          "sequenceLength",
          "sequencesFlaggedAsPoorQuality",
          "totalReads",
          "rrna",
          "rrnaRate")
    )
    # Column classes
    expect_equal(
        vapply(metrics, class, FUN.VALUE = "character"),
        c(sampleID = "character",
          sampleName = "character",
          description = "character",
          xGC = "numeric",
          x53Bias = "numeric",
          averageInsertSize = "numeric",
          duplicates = "numeric",
          duplicationRateOfMapped = "numeric",
          exonicRate = "numeric",
          intergenicRate = "numeric",
          intronicRate = "numeric",
          mappedPairedReads = "numeric",
          mappedReads = "numeric",
          name = "character",
          qualityFormat = "character",
          sequenceLength = "character",
          sequencesFlaggedAsPoorQuality = "numeric",
          totalReads = "numeric",
          rrna = "numeric",
          rrnaRate = "numeric")
    )
})
