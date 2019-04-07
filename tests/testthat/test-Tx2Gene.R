context("Tx2Gene")

object <- Tx2Gene(txse)

test_that("Tx2Gene", {
    expect_s4_class(object, "Tx2Gene")
    expect_identical(dim(object), c(6L, 2L))
})

test_that("summary", {
    output <- capture.output(summary(object))
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
