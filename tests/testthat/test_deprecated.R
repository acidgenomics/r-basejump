context("deprecated")

test_that("Hard deprecations", {
    hard <- c(
        "summarizeRows",
        "wash",
        "packageSE",
        "prepareSE",
        "metadataTable",
        "sampleDirs",
        "comp",
        "revcomp",
        "symbol2gene"
    )
    invisible(lapply(
        X = hard,
        FUN = function(name) {
            fun <- get(name)
            expect_warning(fun(), "'fun' is deprecated.")
        }
    ))
})

test_that("Soft deprecations", {
    soft <- c(
        "pct",
        "fc2lr",
        "lr2fc"
    )
    invisible(lapply(
        X = soft,
        FUN = function(name) {
            fun <- get(name)
            expect_error(suppressWarnings(fun()))
        }
    ))
})
