context("upperCamel")

loadRemoteData(
    file.path(testDataURL, "makeNames.rda"),
    quiet = TRUE)

test_that("character", {
    expect_equal(
        upperCamel(makeNames[["vec"]], strict = FALSE),
        c("HelloWorld",
          "HELLOWORLD",  # improve this?
          "RNAIClones",
          "NCount",
          "Tx2gene",
          "TX2GeneID",
          "G2MScore",
          "WorfdbHTMLRemap",
          "MazdaRX4",
          "X123",
          NA)
    )
    expect_equal(
        upperCamel(makeNames[["vec"]], strict = TRUE),
        c("HelloWorld",
          "HelloWorld",
          "RnaiClones",
          "NCount",
          "Tx2gene",
          "Tx2GeneId",
          "G2mScore",
          "WorfdbHtmlRemap",
          "MazdaRx4",
          "X123",
          NA)
    )
})

test_that("named character", {
    expect_equal(
        upperCamel(makeNames[["namedVec"]], strict = TRUE),
        c(ItemA = "hello world",
          ItemB = "HELLO WORLD")
    )
})

test_that("data.frame", {
    # data.frame with rownames
    expect_equal(
        upperCamel(makeNames[["df"]], rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "MazdaRx4"
    )

    # data.frame with unset rownames (ignore in `.checkRownames()`)
    expect_equal(
        mtcars %>%
            magrittr::set_rownames(., NULL) %>%
            upperCamel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(mtcars))
    )
})

test_that("tibble", {
    expect_equal(
        makeNames[["tbl"]] %>%
            .[, 1L:5L] %>%
            upperCamel(strict = TRUE) %>%
            colnames(),
        c("Name", "Height", "Mass", "HairColor", "SkinColor")
    )
})

test_that("named list", {
    expect_equal(
        upperCamel(makeNames[["lst"]], strict = TRUE),
        list(ItemA = c(1L, 2L),
             ItemB = c(3L, 4L))
    )
})

test_that("missing", {
    expect_error(
        upperCamel(),
        "argument \"object\" is missing, with no default"
    )
})
