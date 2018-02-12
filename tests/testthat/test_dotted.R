context("dotted")

load(system.file(
    file.path("extdata", "makeNames.rda"),
    package = "basejump"))

test_that("Character", {
    expect_identical(
        dotted(makeNames[["character"]]),
        c("hello.world",
          "HELLO.WORLD",
          "RNAI.clones",
          "n.Count",
          "tx2gene",
          "TX2.Gene.ID",
          "G2M.Score",
          "worfdb.HTML.Remap",
          "Mazda.RX4",
          "X123",
          NA)
    )
})

test_that("Named character", {
    expect_identical(
        dotted(makeNames[["namedCharacter"]]),
        c("Item.A" = "hello.world",
          "Item.B" = "HELLO.WORLD")
    )
})

test_that("Data frame", {
    expect_identical(
        dotted(makeNames[["dataFrame"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "Mazda.RX4"
    )
})

test_that("Counts matrix", {
    loadRemoteData("http://basejump.seq.cloud/counts.rda", quiet = TRUE)
    counts <- dotted(counts)
    expect_identical(
        rownames(counts)[[1L]],
        "ENSMUSG00000002459"
    )
    expect_identical(
        colnames(counts),
        c("group1.1", "group1.2", "group2.1", "group2.2")
    )
})

test_that("Matrix rownames", {
    # Sanitize rownames
    expect_identical(
        dotted(makeNames[["matrix"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "Mazda.RX4"
    )
    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        makeNames[["matrix"]] %>%
            set_rownames(NULL) %>%
            dotted(rownames = TRUE) %>%
            rownames(),
        NULL
    )
})

test_that("Tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            dotted() %>%
            colnames(),
        c("name",
          "height",
          "mass",
          "hair.color",
          "skin.color")
    )
})

test_that("List", {
    expect_identical(
        dotted(makeNames[["list"]]),
        list(Item.A = c(1L, 2L),
             Item.B = c(3L, 4L))
    )
})
