context("Deprecated Functions")

test_that("Defunct", {
    x <- c(
        "summarizeRows",
        "wash",
        "packageSE",
        "prepareSE",
        "metadataTable",
        "comp",
        "revcomp",
        "symbol2gene",
        "annotable"
    )
    invisible(lapply(
        X = x,
        FUN = function(x) {
            fun <- get(x)
            expect_error(fun(), "'fun' is defunct.")
        }
    ))
})

test_that("Deprecated", {
    x <- c(
        "pct",
        "fc2lr",
        "lr2fc",
        "checkAnnotable",
        "checkGene2symbol",
        "checkTx2gene",
        "assertFormalHeaderLevel",
        "assertFormalColorFunction",
        "initializeDir"
    )
    invisible(lapply(
        X = x,
        FUN = function(x) {
            fun <- get(x)
            output <- tryCatch(
                fun(),
                warning = function(w) w
            )
            expect_true(grepl("deprecated", output))
        }
    ))
})
