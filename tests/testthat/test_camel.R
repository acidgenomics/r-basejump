context("camel")

loadRemoteData(
    "http://basejump.seq.cloud/makeNames.rda",
    quiet = TRUE)

test_that("character", {
    expect_equal(
        camel(makeNames[["vec"]], strict = FALSE),
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
    expect_equal(
        camel(makeNames[["vec"]], strict = TRUE),
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

test_that("named character", {
    expect_equal(
        camel(makeNames[["namedVec"]], strict = TRUE),
        c(itemA = "hello world",
          itemB = "HELLO WORLD")
    )
})

test_that("data.frame", {
    # data.frame with rownames
    expect_equal(
        camel(makeNames[["df"]], rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazdaRx4"
    )

    # data.frame with unset rownames (ignore in `.checkRownames()`)
    expect_equal(
        mtcars %>%
            magrittr::set_rownames(., NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(mtcars))
    )
})

test_that("tibble", {
    expect_equal(
        makeNames[["tbl"]] %>%
            .[, 1L:5L] %>%
            camel(strict = TRUE) %>%
            colnames(),
        c("name", "height", "mass", "hairColor", "skinColor")
    )
})

test_that("named list", {
    expect_equal(
        camel(makeNames[["lst"]], strict = TRUE),
        list(itemA = c(1L, 2L),
             itemB = c(3L, 4L))
    )
})

test_that("missing", {
    expect_error(
        camel(),
        "argument \"object\" is missing, with no default"
    )
})
