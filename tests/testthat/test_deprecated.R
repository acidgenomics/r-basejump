context("Deprecated Functions")

test_that("snake_case", {
    expect_warning(
        save_data(),
        paste("'save_data' is deprecated.",
              "Use 'saveData' instead.",
              sep = "\\n"))
    expect_warning(
        write_counts(mtcars),
        paste("'write_counts' is deprecated.",
              "Use 'writeCounts' instead.",
              sep = "\\n"))
})



test_that("0.0.19", {
    expect_warning(
        dotNotation(),
        paste("'dotNotation' is deprecated.",
              "Use 'dotted' instead.",
              sep = "\\n"))
    expect_warning(
        grepToString(),
        paste("'grepToString' is deprecated.",
              "Use 'grepString' instead.",
              sep = "\\n"))
    expect_warning(
        loadDataRaw(),
        paste("'loadDataRaw' is deprecated.",
              "Use 'loadData' instead.",
              sep = "\\n"))
    expect_warning(
        toStringSummarize(),
        paste("'toStringSummarize' is deprecated.",
              "Use 'collapse' instead.",
              sep = "\\n"))
})



test_that("0.0.20", {
    expect_warning(
        assignAsNewEnv(),
        paste("'assignAsNewEnv' is deprecated.",
              "Use 'assignIntoNewEnv' instead.",
              sep = "\\n"))
    expect_warning(
        getObjsFromDots(),
        paste("'getObjsFromDots' is deprecated.",
              "Use 'dots' instead.",
              sep = "\\n"))
    expect_warning(
        readDataRaw(),
        "'readDataRaw' is deprecated.")
    expect_warning(
        sanitizeNames(),
        paste("'sanitizeNames' is deprecated.",
              "Use 'snake' instead.",
              sep = "\\n"))
    expect_warning(
        saveDataRaw(),
        "'saveDataRaw' is deprecated.")
    expect_warning(
        wash(),
        "'wash' is deprecated.")
})
