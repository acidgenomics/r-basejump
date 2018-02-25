context("Sanitize Functions")

# fixNA ========================================================================
test_that("fixNA", {
    # character vector
    expect_identical(
        fixNA(c(1L, "x", "", "NA")),
        c("1", "x", NA, NA))

    # data.frame
    expect_identical(
        data.frame(
            "a" = c("foo", ""),
            "b" = c(NA, "bar"),
            stringsAsFactors = FALSE) %>%
            fixNA(),
        data.frame(
            "a" = c("foo", NA),
            "b" = c(NA, "bar"),
            stringsAsFactors = FALSE)
    )

    # DataFrame
    expect_identical(
        DataFrame(a = c("foo", ""),
                  b = c(NA, "bar")) %>%
            fixNA(),
        DataFrame(a = c("foo", NA),
                  b = c(NA, "bar")))

    # tbl_df
    expect_identical(
        tibble(a = c("foo", ""),
               b = c(NA, "bar")) %>%
            fixNA(),
        tibble(a = c("foo", NA),
               b = c(NA, "bar")))

    # ANY (list)
    expect_identical(
        fixNA(list(a = 1L)),
        list(a = 1L))
})



# removeNA =====================================================================
test_that("removeNA : data.frame", {
    # data.frame
    expect_identical(
        data.frame(a = c("A", NA, "C"),
                   b = c(NA, NA, NA),
                   c = c("B", NA, "D")) %>%
            removeNA(),
        data.frame(a = c("A", "C"),
                   c = c("B", "D"),
                   row.names = c(1L, 3L))
    )
})

# Support for vectors (using `stats::na.omit()`)
test_that("removeNA : character", {
    expect_identical(
        removeNA(c("hello", "world", NA)) %>%
            as.character(),
        c("hello", "world")
    )
    expect_identical(
        removeNA(c(1L, 2L, NA)) %>%
            as.integer(),
        c(1L, 2L)
    )
})



# sanitizeAnnotable ============================================================
test_that("sanitizeAnnotable", {
    human <- annotable("Homo sapiens")
    sanitized <- sanitizeAnnotable(human)
    expect_identical(
        lapply(sanitized, class),
        list(
            "ensgene" = "character",
            "symbol" = "character",
            "description" = "character",
            "biotype" = "character",
            "broadClass" = "character",
            "geneSeqStart" = "integer",
            "geneSeqEnd" = "integer",
            "seqName" = "character",
            "seqStrand" = "integer",
            "seqCoordSystem" = "character"
        )
    )
})



# sanitizeColData ==============================================================
test_that("sanitizeColData", {
    x <- coldata
    x[["day"]] <- c(14L, 14L, 30L, 30L)
    x <- sanitizeColData(x)
    expect_is(x, "DataFrame")
    expect_identical(rownames(x), rownames(coldata))
    expect_true(all(vapply(
        X = x,
        FUN = is.factor,
        FUN.VALUE = logical(1L))))
})
