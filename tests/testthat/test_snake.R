context("snake")

suppressMessages(
    loadRemoteData(file.path(testDataURL, "makeNames.rda"))
)

test_that("character", {
    expect_equal(
        snake(makeNames[["vec"]]),
        c("hello_world",
          "hello_world",
          "rnai_clones",
          "tx2gene",
          "tx2_gene_id",
          "g2m_score",
          "worfdb_html_remap",
          "x123",
          NA)
    )
})

test_that("named character", {
    expect_equal(
        snake(makeNames[["namedVec"]]),
        c(item_a = "hello world",
          item_b = "HELLO WORLD")
    )
})

test_that("data.frame", {
    expect_equal(
        snake(makeNames[["df"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazda_rx4"
    )
})

test_that("tibble", {
    expect_equal(
        makeNames[["tbl"]] %>%
            .[, 1L:5L] %>%
            snake() %>%
            colnames(),
        c("name",
          "height",
          "mass",
          "hair_color",
          "skin_color")
    )
})

test_that("named list", {
    expect_equal(
        snake(makeNames[["lst"]]),
        list(item_a = c(1L, 2L),
             item_b = c(3L, 4L))
    )
})

test_that("missing", {
    expect_error(
        snake(),
        "argument \"object\" is missing, with no default"
    )
})
