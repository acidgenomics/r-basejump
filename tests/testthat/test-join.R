context("join : Beatles vs. Stones")

## nolint start
## Compare with `data.frame` objects:
## > data(band_members, band_instruments, package = "dplyr")
## nolint end

data(
    band_members,
    band_instruments,
    package = "acidtest",
    envir = environment()
)

x <- as(band_members, "DataFrame")
y <- as(band_instruments, "DataFrame")
rownames(x) <- x[["name"]]
rownames(y) <- y[["name"]]
by <- "name"

test_that("innerJoin", {
    object <- innerJoin(x = x, y = y, by = by)
    expected <- DataFrame(
        name = c("John", "Paul"),
        band = c("Beatles", "Beatles"),
        plays = c("guitar", "bass"),
        row.names = c("John", "Paul")
    )
    expect_identical(object, expected)
})

test_that("leftJoin", {
    object <- leftJoin(x = x, y = y, by = by)
    expected <- DataFrame(
        name = c("Mick", "John", "Paul"),
        band = c("Stones", "Beatles", "Beatles"),
        plays = c(NA, "guitar", "bass"),
        row.names = c("Mick", "John", "Paul")
    )
    expect_identical(object, expected)
})

test_that("rightJoin", {
    object <- rightJoin(x = x, y = y, by = by)
    expected <- DataFrame(
        name = c("John", "Paul", "Keith"),
        plays = c("guitar", "bass", "guitar"),
        band = c("Beatles", "Beatles", NA),
        row.names = c("John", "Paul", "Keith")
    )
    expect_identical(object, expected)
})

test_that("fullJoin", {
    object <- fullJoin(x = x, y = y, by = by)
    expected <- DataFrame(
        name = c("Mick", "John", "Paul", "Keith"),
        band = c("Stones", "Beatles", "Beatles", NA),
        plays = c(NA, "guitar", "bass", "guitar"),
        row.names = c("Mick", "John", "Paul", "Keith")
    )
    expect_identical(object, expected)
})

test_that("semiJoin", {
    object <- semiJoin(x = x, y = y, by = by)
    expected <- DataFrame(
        name = c("John", "Paul"),
        band = c("Beatles", "Beatles"),
        row.names = c("John", "Paul")
    )
    expect_identical(object, expected)
})

test_that("antiJoin", {
    object <- antiJoin(x = x, y = y, by = by)
    expected <- DataFrame(
        name = "Mick",
        band = "Stones",
        row.names = "Mick"
    )
    expect_identical(object, expected)
})



context("join : bioinfo")

test_that("Matched rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(4L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    expect_identical(
        object = leftJoin(df1, df2, by = "id"),
        expected = DataFrame(
            id = as.factor(seq(4L)),
            genotype = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
        )
    )
})

test_that("Unmatched rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(4L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    ## Reverse the row order of df2.
    df2 <- df2[rev(seq_len(nrow(df2))), ]
    expect_identical(
        object = leftJoin(df1, df2, by = "id"),
        expected = DataFrame(
            id = as.factor(seq(4L)),
            genotype = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
        )
    )
})

test_that("Uneven rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(2L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 1L))
    )
    expect_identical(
        object = leftJoin(df1, df2, by = "id"),
        expected = DataFrame(
            id = as.factor(seq(4L)),
            genotype = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            treatment = as.factor(c("control", "expt", NA, NA))
        )
    )
})
