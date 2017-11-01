context("prepareAnnotable")

# Use the pre-compiled grch37 annotable from annotables package
loadRemoteData("http://basejump.seq.cloud/grch37.rda")

test_that("Drop extra columns", {
    # This is fast but will drop extra columns
    drop <- prepareAnnotable(grch37, dropExtraCols = TRUE)
    expect_equal(
        dim(drop),
        c(63677, 5)
    )
    expect_equal(
        colnames(drop),
        c("ensgene",
          "symbol",
          "description",
          "biotype",
          "broadClass")
    )
})

test_that("Nest extra columns", {
    # This is slow but will keep the extra columns by nesting
    nest <- prepareAnnotable(grch37, dropExtraCols = FALSE)
    expect_equal(
        dim(nest),
        c(63677, 6)
    )
    expect_equal(
        colnames(nest),
        c("ensgene",
          "symbol",
          "description",
          "biotype",
          "nestedData",
          "broadClass")
    )
})

test_that("Malformed annotable pass-in", {
    malformed <- grch37[, c("ensgene", "symbol")]
    expect_error(
        prepareAnnotable(malformed),
        "Required columns: ensgene, symbol, description, biotype"
    )
})
