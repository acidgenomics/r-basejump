context("Sanitization Functions")

groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
mpgString <- "18.1, 18.7, 21, 21.4, 22.8"



# collapseToString =============================================================
test_that("collapseToString : atomic", {
    expect_identical(
        collapseToString(
            groceries,
            sort = TRUE,
            removeNA = FALSE,
            unique = FALSE
        ),
        "eggs, eggs, milk, veggies, NA, NA"
    )
    expect_identical(
        collapseToString(
            groceries,
            sort = TRUE,
            removeNA = TRUE,
            unique = FALSE
        ),
        "eggs, eggs, milk, veggies"
    )
    expect_identical(
        collapseToString(
            groceries,
            sort = TRUE,
            removeNA = TRUE,
            unique = TRUE
        ),
        "eggs, milk, veggies"
    )
})

test_that("colllapseToString : data.frame", {
    expect_identical(
        datasets::mtcars %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[, "mpg", drop = TRUE],
        mpgString
    )
})

test_that("collapseToString : DataFrame", {
    expect_identical(
        mtcars %>%
            as("DataFrame") %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[, "mpg"],
        mpgString
    )
})

test_that("collapseToString : integer", {
    expect_identical(
        collapseToString(seq(1L:5L)),
        "1, 2, 3, 4, 5"
    )
})

test_that("collapseToString : logical", {
    expect_identical(
        collapseToString(c(TRUE, FALSE), sort = TRUE),
        "FALSE, TRUE"
    )
    # NA and NaN should stay fixed even when sorting is enabled
    expect_identical(
        collapseToString(c(NA, NaN), sort = TRUE),
        "NA, NaN"
    )
    expect_identical(
        collapseToString(c(NaN, NA), sort = TRUE),
        "NaN, NA"
    )
})

test_that("collapseToString : matrix", {
    expect_identical(
        mtcars %>%
            as("matrix") %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[1L, "mpg", drop = TRUE] %>%
            as.character(),
        mpgString
    )
})

test_that("collapseToString : numeric", {
    expect_identical(
        collapseToString(c(3.141593, 6.0221409e+23)),
        "3.141593, 6.0221409e+23"
    )
})

test_that("collapseToString : scalar early return", {
    expect_identical(collapseToString(1L), 1L)
})



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
            row.names = c("c", "d"),
            stringsAsFactors = FALSE
        ) %>%
            fixNA(),
        data.frame(
            "a" = c("foo", NA),
            "b" = c(NA, "bar"),
            row.names = c("c", "d"),
            stringsAsFactors = FALSE
        )
    )

    # DataFrame
    expect_identical(
        DataFrame(
            "a" = c("foo", ""),
            "b" = c(NA, "bar")
        ) %>%
            fixNA(),
        DataFrame(
            "a" = c("foo", NA),
            "b" = c(NA, "bar")
        )
    )

    # tbl_df
    expect_identical(
        tibble(
            "a" = c("foo", ""),
            "b" = c(NA, "bar")
        ) %>%
            fixNA(),
        tibble(
            "a" = c("foo", NA),
            "b" = c(NA, "bar")
        )
    )

    # ANY (list)
    expect_identical(
        fixNA(list("a" = 1L)),
        list("a" = 1L)
    )
})



# removeNA =====================================================================
test_that("removeNA : data.frame", {
    # data.frame
    expect_identical(
        data.frame(
            "a" = c("A", NA, "C"),
            "b" = c(NA, NA, NA),
            "c" = c("B", NA, "D")
        ) %>%
            removeNA(),
        data.frame(
            "a" = c("A", "C"),
            "c" = c("B", "D"),
            row.names = c(1L, 3L)
        )
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



# sanitizeRowData ==============================================================
test_that("sanitizeRowData", {
    x <- makeGRangesFromEnsembl("Homo sapiens")
    x <- sanitizeRowData(x)
    expect_identical(
        lapply(x, class),
        list(
            "rowname" = "character",
            "seqnames" = "factor",
            "start" = "integer",
            "end" = "integer",
            "width" = "integer",
            "strand" = "factor",
            "geneID" = "character",
            "geneName" = "character",
            "geneBiotype" = "factor",
            "description" = "character",
            "seqCoordSystem" = "factor",
            "broadClass" = "factor"
        )
    )
})



# sanitizeSampleData ===========================================================
test_that("sanitizeSampleData", {
    x <- sanitizeSampleData(sd)
    expect_is(x, "DataFrame")
    expect_identical(rownames(x), rownames(sd))
    expect_true(all(vapply(
        X = x,
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    )))
})
