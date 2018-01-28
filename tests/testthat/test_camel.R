context("camel")

load(system.file(
    file.path("extdata", "makeNames.rda"),
    package = "basejump"))

test_that("character", {
    expect_identical(
        camel(makeNames[["character"]], strict = FALSE),
        c("helloWorld",
          "helloWORLD",
          "rnaiClones",
          "nCount",
          "tx2gene",
          "tx2GeneID",
          "g2mScore",
          "worfdbHTMLRemap",
          "mazdaRX4",
          "x123",
          NA)
    )
    expect_identical(
        camel(makeNames[["character"]], strict = TRUE),
        c("helloWorld",
          "helloWorld",
          "rnaiClones",
          "nCount",
          "tx2gene",
          "tx2GeneId",
          "g2mScore",
          "worfdbHtmlRemap",
          "mazdaRx4",
          "x123",
          NA)
    )
})

test_that("Delimited numbers", {
    expect_identical(
        camel("2018-01-01"),
        "x2018x01x01"
    )
    expect_identical(
        camel("0.01"),
        "x0x01"
    )
    expect_identical(
        camel("1,000,000"),
        "x1x000x000"
    )
})

test_that("named character", {
    expect_identical(
        camel(makeNames[["namedCharacter"]], strict = TRUE),
        c("itemA" = "helloWorld",
          "itemB" = "helloWorld")
    )
})

test_that("data.frame", {
    # Sanitize rownames
    expect_identical(
        camel(makeNames[["dataFrame"]], rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazdaRx4"
    )
    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        makeNames[["dataFrame"]] %>%
            magrittr::set_rownames(., NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(makeNames[["dataFrame"]]))
    )
})

test_that("matrix", {
    # Sanitize rownames
    expect_identical(
        camel(makeNames[["matrix"]], rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazdaRx4"
    )
    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        makeNames[["matrix"]] %>%
            magrittr::set_rownames(., NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        NULL
    )
})

test_that("tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            camel(strict = TRUE) %>%
            colnames(),
        c("name", "height", "mass", "hairColor", "skinColor")
    )
})

test_that("named list", {
    expect_identical(
        camel(makeNames[["list"]], strict = TRUE),
        list("itemA" = c(1L, 2L),
             "itemB" = c(3L, 4L))
    )
})

test_that("missing", {
    expect_error(
        camel(),
        "argument \"object\" is missing, with no default"
    )
})
