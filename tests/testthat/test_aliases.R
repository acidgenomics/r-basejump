context("aliases")

test_that("aliases", {
    aliases <- c(
        "gene2symbolFromGTF" = "gene2symbolFromGFF",
        "geomean" = "geometricMean",
        "readGTF" = "readGFF",
        "tx2geneFromGTF" = "tx2geneFromGFF"
    )
    invisible(lapply(
        X = seq_along(aliases),
        FUN = function(a) {
            x <- aliases[[a]]
            y <- names(aliases)[[a]] %>%
                get() %>%
                deparse() %>%
                .[[3L]] %>%
                gsub(" ", "", .) %>%
                gsub("\\(\\.\\.\\.\\)$", "", .)
            expect_identical(x, y)
        }
    ))
})
