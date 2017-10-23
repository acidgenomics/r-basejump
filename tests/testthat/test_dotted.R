context("dotted")

loadRemoteData(
    "http://basejump.seq.cloud/makeNames.rda",
    quiet = TRUE)

test_that("character", {
    expect_equal(
        dotted(makeNames[["vec"]]),
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
    expect_equal(
        dotted(makeNames[["namedVec"]]),
        c(Item.A = "hello world",
          Item.B = "HELLO WORLD")
    )
})

test_that("data.frame", {
    expect_equal(
        dotted(makeNames[["df"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "Mazda.RX4"
    )
})

test_that("tibble", {
    expect_equal(
        makeNames[["tbl"]] %>%
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

test_that("named list", {
    expect_equal(
        dotted(makeNames[["lst"]]),
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
