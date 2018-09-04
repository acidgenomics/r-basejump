context("Sanitization Functions")

groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
mpgString <- "18.1, 18.7, 21, 21.4, 22.8"



# collapseToString =============================================================
test_that("collapseToString : atomic", {
    expect_identical(
        object = collapseToString(
            object = groceries,
            sort = TRUE,
            removeNA = FALSE,
            unique = FALSE
        ),
        expected = "eggs, eggs, milk, veggies, NA, NA"
    )
    expect_identical(
        object = collapseToString(
            object = groceries,
            sort = TRUE,
            removeNA = TRUE,
            unique = FALSE
        ),
        expected = "eggs, eggs, milk, veggies"
    )
    expect_identical(
        object = collapseToString(
            object = groceries,
            sort = TRUE,
            removeNA = TRUE,
            unique = TRUE
        ),
        expected = "eggs, milk, veggies"
    )
})

test_that("colllapseToString : data.frame", {
    expect_identical(
        object = datasets::mtcars %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[, "mpg", drop = TRUE],
        expected = mpgString
    )
})

test_that("collapseToString : DataFrame", {
    expect_identical(
        object = mtcars %>%
            as("DataFrame") %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[, "mpg"],
        expected = mpgString
    )
})

test_that("collapseToString : integer", {
    expect_identical(
        object = collapseToString(seq(1L:5L)),
        expected = "1, 2, 3, 4, 5"
    )
})

test_that("collapseToString : logical", {
    expect_identical(
        object = collapseToString(c(TRUE, FALSE), sort = TRUE),
        expected = "FALSE, TRUE"
    )
    # NA and NaN should stay fixed even when sorting is enabled
    expect_identical(
        object = collapseToString(c(NA, NaN), sort = TRUE),
        expected = "NA, NaN"
    )
    expect_identical(
        object = collapseToString(c(NaN, NA), sort = TRUE),
        expected = "NaN, NA"
    )
})

test_that("collapseToString : matrix", {
    expect_identical(
        object = mtcars %>%
            as("matrix") %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[1L, "mpg", drop = TRUE] %>%
            as.character(),
        expected = mpgString
    )
})

test_that("collapseToString : numeric", {
    expect_identical(
        object = collapseToString(c(3.141593, 6.0221409e+23)),
        expected = "3.141593, 6.0221409e+23"
    )
})

test_that("collapseToString : scalar early return", {
    expect_identical(collapseToString(1L), 1L)
})



# removeNA =====================================================================
# Support for vectors (using `stats::na.omit()`).
# This will return structure attributes about original size, with class omit.
with_parameters_test_that(
    "removeNA", {
        expect_identical(
            object = removeNA(object),
            expected = expected
        )
    },
    object = list(
        character = c("hello", "world", NA),
        numeric = c(1L, 2L, NA),
        DataFrame = DataFrame(
            a = c("A", NA, "C"),
            b = c(NA, NA, NA),
            c = c("B", NA, "D")
        )
    ),
    expected = list(
        character = structure(
            .Data = c("hello", "world"),
            na.action = structure(3L, class = "omit")
        ),
        numeric = structure(
            .Data = c(1L, 2L),
            na.action = structure(3L, class = "omit")
        ),
        DataFrame = DataFrame(
            a = c("A", "C"),
            c = c("B", "D"),
            row.names = c(1L, 3L)
        )
    )
)



# sanitizeNA ===================================================================
with_parameters_test_that(
    "sanitizeNA", {
        expect_identical(
            object = sanitizeNA(object),
            expected = expected
        )
    },
    object = list(
        character = c(1L, "x", "", "NA"),
        data.frame = data.frame(
            a = c("foo", ""),
            b = c(NA, "bar"),
            stringsAsFactors = FALSE
        ),
        DataFrame1 = DataFrame(
            a = c("foo", ""),
            b = c(NA, "bar"),
            row.names = c("c", "d")
        ),
        DataFrame2 = DataFrame(
            a = c("foo", ""),
            b = c(NA, "bar")
        ),
        tbl_df = tibble(
            a = c("foo", ""),
            b = c(NA, "bar")
        )
    ),
    expected = list(
        character = c("1", "x", NA, NA),
        data.frame = data.frame(
            a = c("foo", NA),
            b = c(NA, "bar"),
            stringsAsFactors = FALSE
        ),
        DataFrame1 = DataFrame(
            a = c("foo", NA),
            b = c(NA, "bar"),
            row.names = c("c", "d")
        ),
        DataFrame2 = DataFrame(
            a = c("foo", NA),
            b = c(NA, "bar")
        ),
        tbl_df = tibble(
            a = c("foo", NA),
            b = c(NA, "bar")
        )
    )
)



# sanitizeRowData ==============================================================
test_that("sanitizeRowData", {
    object <- sanitizeRowData(rowRanges(rse_small))
    expect_identical(
        object = lapply(object, class),
        expected = list(
            rowname = "character",
            seqnames = "factor",
            start = "integer",
            end = "integer",
            width = "integer",
            strand = "factor",
            broadClass = "factor",
            description = "factor",
            geneBiotype = "factor",
            geneID = "character",
            geneName = "factor",
            seqCoordSystem = "factor"
        )
    )
})



# sanitizeSampleData ===========================================================
with_parameters_test_that(
    "sanitizeSampleData", {
        # `sampleName` column is required.
        object[["sampleName"]] <- NULL
        expect_error(
            object = sanitizeSampleData(object),
            regexp = "sampleName"
        )

        # And `sampleName` column can't contain duplicates.
        object[["sampleName"]] <- "XXX"
        expect_error(
            object = sanitizeSampleData(object),
            regexp = "has_no_duplicates"
        )

        # All columns should return factor.
        object[["sampleName"]] <- paste("sample", seq_len(nrow(object)))
        data <- sanitizeSampleData(object)
        expect_is(data, "DataFrame")
        expect_true(all(vapply(
            X = data,
            FUN = is.factor,
            FUN.VALUE = logical(1L)
        )))


    },
    object = list(
        DataFrame = DataFrame(
            genotype = rep(c("wt", "ko"), 2L),
            batch = c(1L, 1L, 2L, 2L),
            row.names = paste("sample", seq_len(4L), sep = "_")
        ),
        tbl_df = tibble(
            rowname = paste("sample", seq_len(4L), sep = "_"),
            sample_name = paste0("patient", seq_len(4L)),
            genotype = c("wt", "ko", "wt", "ko"),
            batch = c(1L, 1L, 2L, 2L)
        )
    )
)
