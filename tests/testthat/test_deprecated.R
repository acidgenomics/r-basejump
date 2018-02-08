context("deprecated")

test_that("Hard deprecations", {
    hard <- c(
        "comp",
        "metadataTable",
        "packageSE",
        "prepareSE",
        "revcomp",
        "sampleDirs",
        "summarizeRows",
        "wash"
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
        "fc2lr",
        "lr2fc",
        "pct"
    )
    invisible(lapply(
        X = soft,
        FUN = function(name) {
            fun <- get(name)
            expect_error(suppressWarnings(fun()))
        }
    ))
})
