context("Data Import and Project Management Utilities")

test_that("packageSE", {
    mat <- mtcars %>%
        .[c("Mazda RX4", "Datsun 710"), ] %>%
        .[, c("mpg", "gear")] %>%
        as.matrix
    coldata <- data.frame(
        description = c("Miles per gallon", "Number of gears"),
        abbreviation = c(TRUE, FALSE),
        row.names = colnames(mat))
    rowdata <- data.frame(
        manufacturer = c("Mazda", "Datsun"),
        model_number = c("RX4", "710"),
        row.names = rownames(mat))
    se <- packageSE(mat, colData = coldata, rowData = rowdata)
    expect_equal(
        dim(se),
        c(2L, 2L))
    expect_equal(
        names(metadata(se)),
        c("date", "wd", "hpc", "sessionInfo"))
})



test_that("transmit", {
    expect_equal(
        transmit("ftp://ftp.ensembl.org/pub/release-89",
                 pattern = "README",
                 rename = "ensembl_readme.txt",
                 compress = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        "data-raw/ensembl_readme.txt.gz")
})
