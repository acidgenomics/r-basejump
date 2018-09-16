context("Math and Science Functions")



# aggregateRows ================================================================
test_that("aggregateRows", {
    expected <- as.matrix(DataFrame(
        "sample1" = c(3L, 7L),
        "sample2" = c(11L, 15L),
        "sample3" = c(19L, 23L),
        "sample4" = c(27L, 31L),
        row.names = c("gene1", "gene2")
    ))

    groupings <- as.factor(paste0("gene", rep(seq_len(2L), each = 2L)))
    names(groupings) <- rownames(mat)

    # matrix
    object <- aggregateRows(mat, groupings = groupings)
    expect_is(object, "matrix")
    expect_identical(object, expected)

    # sparseMatrix
    object <- aggregateRows(sparse, groupings = groupings)
    expect_is(object, "sparseMatrix")
    # Is there a way to improve this check?
    expect_equal(
        object = as.matrix(object),
        expected = expected
    )

    # Invalid groupings
    expect_error(
        object = aggregateRows(mat, groupings = "XXX"),
        regexp = "is_factor :"
    )
    expect_error(
        object = aggregateRows(mat, groupings = factor(c("XXX", "YYY"))),
        regexp = "are_identical :"
    )
})



# aggregateCols ================================================================
test_that("aggregateCols", {
    expected <- as.matrix(DataFrame(
        "sample1" = c(6L, 8L, 10L, 12L),
        "sample2" = c(22L, 24L, 26L, 28L),
        row.names = rownames(mat)
    ))

    groupings <- as.factor(paste0("sample", rep(seq_len(2L), each = 2L)))
    names(groupings) <- colnames(df)

    # matrix
    object <- aggregateCols(mat, groupings = groupings)
    expect_is(object, "matrix")
    expect_identical(object, expected)

    # sparseMatrix
    object <- aggregateCols(sparse, groupings = groupings)
    expect_is(object, "sparseMatrix")
    # Is there a way to improve this check?
    expect_equal(
        object = as.matrix(object),
        expected = expected
    )

    # Invalid groupings
    expect_error(
        object = aggregateCols(mat, groupings = "XXX"),
        regexp = "is_factor :"
    )
    expect_error(
        object = aggregateCols(mat, groupings = factor(c("XXX", "YYY"))),
        regexp = "are_identical :"
    )
})



# FIXME Parameterize this test.
# geometricMean ================================================================
test_that("geometricMean", {
    int <- seq(from = 1L, to = 5L, by = 1L)
    num <- int ^ 2L
    df <- data.frame(int, num)
    mat <- as.matrix(df)
    mean <- c(int = 2.605171, num = 6.786916)

    # integer ==================================================================
    expect_identical(
        round(geometricMean(int), digits = 6L),
        mean[["int"]]
    )

    # numeric ==================================================================
    expect_identical(
        round(geometricMean(num), digits = 6L),
        mean[["num"]]
    )

    # matrix ===================================================================
    expect_identical(
        round(geometricMean(mat), digits = 6L),
        mean
    )

    # NaN on negative numbers ==================================================
    expect_identical(
        geometricMean(seq(from = -5L, to = 5L, by = 1L)),
        NaN
    )

    # Zero propagation =========================================================
    expect_identical(
        geometricMean(
            seq(from = 0L, to = 5L, by = 1L),
            zeroPropagate = TRUE
        ),
        0L
    )
    expect_identical(
        round(geometricMean(
            seq(from = 1L, to = 5L, by = 1L),
            zeroPropagate = TRUE
        ), digits = 6L),
        2.605171
    )
})



# logRatio =====================================================================
test_that("logRatio", {
    fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
    lr <- seq(-3L, 3L, 1L)
    vec1 <- seq(from = 1L, to = 5L, by = 1L)
    vec2 <- vec1 ^ 2L
    means <- c(vec1 = 2.605171, vec2 = 6.786916)
    expect_equal(foldChangeToLogRatio(fc), lr)
    expect_equal(logRatioToFoldChange(lr), fc)
    expect_error(
        foldChangeToLogRatio(lr, base = 0L),
        "is_positive : base"
    )
    expect_error(
        logRatioToFoldChange(lr, base = 0L),
        "is_positive : base"
    )
})



# microplate ===================================================================
test_that("microplate : 96-well plate format", {
    plate <- microplate(plates = 1L, wells = 96L)
    expect_is(plate, "character")
    expect_identical(length(plate), 96L)
    expect_identical(
        head(plate),
        c("1-A01", "1-A02", "1-A03", "1-A04", "1-A05", "1-A06")
    )
    expect_identical(
        tail(plate),
        c("1-H07", "1-H08", "1-H09", "1-H10", "1-H11", "1-H12")
    )
})

test_that("microplate : 384-well plate format", {
    plate <- microplate(plates = 1L, wells = 384L)
    expect_is(plate, "character")
    expect_identical(
        tail(plate),
        c("1-P19", "1-P20", "1-P21", "1-P22", "1-P23", "1-P24")
    )
})

test_that("microplate : Multiple plates", {
    expect_identical(
        microplate(plates = 2L, wells = 96L) %>%
            length(),
        192L
    )
    expect_identical(
        microplate(plates = 2L, wells = 384L) %>%
            length(),
        768L
    )
})

test_that("microplate : Control wells", {
    plate <- microplate(controls = 3L)
    expect_identical(plate[[1L]], "1-A04")
})

test_that("microplate : Prefix", {
    plates <- microplate(prefix = "cherrypick")
    expect_identical(plates[[1L]], "cherrypick-1-A01")
})

test_that("microplate : Invalid parameters", {
    expect_error(
        microplate(plates = -1L),
        "is_positive : plates"
    )
    expect_error(
        microplate(wells = 4L),
        "is_subset : The element '4' in wells is not in c\\(96L, 384L\\)."
    )
    expect_error(
        microplate(controls = -1L),
        "is_non_negative : controls"
    )
    expect_error(
        microplate(prefix = c("a", "b")),
        "is_a_string :"
    )
})
