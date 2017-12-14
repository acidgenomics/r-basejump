context("dotted")

load(system.file(
    file.path("extdata", "makeNames.rda"),
    package = "basejump"))

test_that("character", {
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

test_that("named_character", {
    expect_identical(
        dotted(makeNames[["namedCharacter"]]),
        c("Item.A" = "hello.world",
          "Item.B" = "HELLO.WORLD")
    )
})

test_that("data.frame", {
    expect_identical(
        dotted(makeNames[["dataFrame"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "Mazda.RX4"
    )
})

test_that("tibble", {
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

test_that("list", {
    expect_identical(
        dotted(makeNames[["list"]]),
        list(Item.A = c(1L, 2L),
             Item.B = c(3L, 4L))
    )
})

test_that("missing", {
    expect_error(
        dotted(),
        "argument \"object\" is missing, with no default"
    )
})
