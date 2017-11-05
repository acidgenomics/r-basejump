context("prepareAnnotable")

# Use the pre-compiled grch37 annotable from annotables package
loadRemoteData("http://basejump.seq.cloud/grch37.rda", quiet = TRUE)

test_that("grch37", {
    # This is fast but will drop extra columns
    human <- prepareAnnotable(grch37)
    expect_equal(
        dim(human),
        c(63677L, 9L)
    )
    expect_equal(
        lapply(human, class),
        list(
            ensgene = "character",
            symbol = "character",
            description = "character",
            biotype = "character",
            broadClass = "character",
            chr = "character",
            start = "integer",
            end = "integer",
            strand = "integer"
        )
    )
})

test_that("Malformed annotable pass-in", {
    malformed <- grch37[, c("ensgene", "symbol")]
    expect_error(
        prepareAnnotable(malformed),
        "Required columns: ensgene, symbol, description, biotype"
    )
})
