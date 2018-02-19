context("upperCamel")

load(system.file("extdata/makeNames.rda", package = "basejump"))

test_that("ANY", {
    # Integer (atomic)
    expect_warning(
        upperCamel(1L),
        "Returning without upper camel case sanitization applied"
    )
    expect_identical(
        upperCamel(c("hello.world" = 1L)),
        c("HelloWorld" = 1L)
    )
})

test_that("Character", {
    expect_identical(
        upperCamel(makeNames[["character"]], strict = FALSE),
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
    expect_identical(
        upperCamel(makeNames[["character"]], strict = TRUE),
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

test_that("Delimited numbers", {
    expect_identical(
        upperCamel("2018-01-01"),
        "X2018x01x01"
    )
    expect_identical(
        upperCamel("0.01"),
        "X0x01"
    )
    expect_identical(
        upperCamel("1,000,000"),
        "X1x000x000"
    )
})

test_that("Named character", {
    expect_identical(
        upperCamel(makeNames[["namedCharacter"]], strict = TRUE),
        c("ItemA" = "HelloWorld",
          "ItemB" = "HelloWorld")
    )
})

test_that("Data frame", {
    # Sanitize rownames
    expect_identical(
        upperCamel(makeNames[["dataFrame"]],
                   rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "MazdaRx4"
    )
    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        mtcars %>%
            set_rownames(NULL) %>%
            upperCamel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(mtcars))
    )
})

test_that("Counts matrix", {
    loadRemoteData("http://basejump.seq.cloud/counts.rda", quiet = TRUE)
    counts <- upperCamel(counts)
    expect_identical(
        rownames(counts)[[1L]],
        "ENSMUSG00000002459"
    )
    expect_identical(
        colnames(counts),
        c("Group1x1", "Group1x2", "Group2x1", "Group2x2")
    )
})

test_that("Matrix rownames", {
    # Sanitize rownames
    expect_identical(
        upperCamel(makeNames[["matrix"]],
                   rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "MazdaRx4"
    )
    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        makeNames[["matrix"]] %>%
            set_rownames(NULL) %>%
            upperCamel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        NULL
    )
})

test_that("Tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            upperCamel(strict = TRUE) %>%
            colnames(),
        c("Name", "Height", "Mass", "HairColor", "SkinColor")
    )
})

test_that("Named list", {
    expect_identical(
        upperCamel(makeNames[["list"]], strict = TRUE),
        list("ItemA" = c(1L, 2L),
             "ItemB" = c(3L, 4L))
    )
})

test_that("Missing", {
    expect_error(
        upperCamel(),
        "argument \"object\" is missing, with no default"
    )
})
