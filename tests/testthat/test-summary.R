context("summary")

test_that("Gene2Symbol", {
    x <- Gene2Symbol(rse)
    output <- capture.output(summary(x))
    expect_identical(
        output,
        c(
            "genes: 500",
            "symbols: 500",
            "format: makeUnique",
            "organism: Homo sapiens",
            "genomeBuild: GRCh38",
            "ensemblRelease: 92",
            "id: AH60977",
            "version: 0.1.0",
            "date: 2019-03-27"
        )
    )
})

test_that("Tx2Gene", {
    x <- Tx2Gene(txse)
    output <- capture.output(summary(x))
    expect_identical(
        output,
        c(
            "transcripts: 6",
            "genes: 2",
            "organism: Homo sapiens",
            "genomeBuild: GRCh38",
            "ensemblRelease: 92",
            "id: AH60977",
            "version: 0.1.0",
            "date: 2019-03-27"
        )
    )
})

