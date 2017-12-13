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
        data.frame(
            "group" = c(
                "ctrl",
                "ctrl",
                "ko",
                "ko"),
            "description" = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2"),
            row.names = c(
                "group1_1",
                "group1_2",
                "group2_1",
                "group2_2"),
            stringsAsFactors = FALSE
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
    samples <- c("group1_1", "group1_2", "group2_1", "group2_2")
    expect_equal(
        sampleYAMLMetadata(yaml),
        data.frame(
            sampleID = factor(samples, levels = samples),
            sampleName = factor(samples, levels = samples),
            description = factor(samples, levels = samples),
            group = factor(c("ctrl", "ctrl", "ko", "ko"),
                           levels = c("ctrl", "ko")),
            row.names = samples
        )
    )
})

test_that("sampleYAMLMetrics", {
    metrics <- sampleYAMLMetrics(yaml)
    expect_equal(
        vapply(metrics, class, FUN.VALUE = "character"),
        c(sampleID = "factor",
          sampleName = "factor",
          description = "factor",
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
          name = "factor",
          qualityFormat = "factor",
          sequenceLength = "factor",
          sequencesFlaggedAsPoorQuality = "numeric",
          totalReads = "numeric",
          rrna = "numeric",
          rrnaRate = "numeric")
    )

    # Check for proper handling of metrics with mismatched number of values
    yaml2 <- readYAML(
        file.path(
            "http://basejump.seq.cloud",
            "bcbio",
            "project-summary-metrics-mismatch.yaml"),
        quiet = TRUE)
    metrics2 <- sampleYAMLMetrics(yaml2)
    expect_equal(
        vapply(metrics2, class, FUN.VALUE = "character"),
        c(sampleID = "factor",
          sampleName = "factor",
          description = "factor",
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
          name = "factor",
          qualityFormat = "factor",
          sequenceLength = "numeric",  # factor in the main example
          sequencesFlaggedAsPoorQuality = "numeric",
          totalReads = "numeric",
          rrna = "numeric",
          rrnaRate = "numeric")
    )
})
