context("camel")

load(system.file(
    file.path("extdata", "makeNames.rda"),
    package = "basejump"))

test_that("ALL", {
    # Integer (atomic)
    expect_warning(
        camel(1L),
        "Returning without lower camel case sanitization applied")
    expect_identical(
        camel(c("hello.world" = 1L)),
        c("helloWorld" = 1L)
    )
})

test_that("Character", {
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

test_that("Named character", {
    expect_identical(
        camel(makeNames[["namedCharacter"]], strict = TRUE),
        c("itemA" = "helloWorld",
          "itemB" = "helloWorld")
    )
})

test_that("Data frame", {
    # Sanitize rownames
    expect_identical(
        camel(makeNames[["dataFrame"]], rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazdaRx4"
    )
    # Unset rownames should be skipped, even when `rownames = TRUE`
    expect_identical(
        makeNames[["dataFrame"]] %>%
            set_rownames(NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(makeNames[["dataFrame"]]))
    )
})

test_that("Counts matrix", {
    loadRemoteData("http://basejump.seq.cloud/counts.rda", quiet = TRUE)
    counts <- camel(counts)
    expect_identical(
        rownames(counts)[[1L]],
        "ENSMUSG00000002459"
    )
    expect_identical(
        colnames(counts),
        c("group1x1", "group1x2", "group2x1", "group2x2")
    )
})

test_that("Matrix rownames", {
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
            set_rownames(NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        NULL
    )
})

test_that("Tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            camel(strict = TRUE) %>%
            colnames(),
        c("name", "height", "mass", "hairColor", "skinColor")
    )
})

test_that("Named list", {
    expect_identical(
        camel(makeNames[["list"]], strict = TRUE),
        list("itemA" = c(1L, 2L),
             "itemB" = c(3L, 4L))
    )
})
