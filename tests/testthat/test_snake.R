context("snake")

load(system.file(
    file.path("extdata", "makeNames.rda"),
    package = "basejump"))

test_that("character", {
    vec <- snake(makeNames[["character"]])
    expect_identical(
        vec,
        c("hello_world",
          "hello_world",
          "rnai_clones",
          "n_count",
          "tx2gene",
          "tx2_gene_id",
          "g2m_score",
          "worfdb_html_remap",
          "mazda_rx4",
          "x123",
          NA)
    )
})

test_that("named character", {
    vec <- snake(makeNames[["namedCharacter"]])
    expect_identical(
        vec,
        c("item_a" = "hello_world",
          "item_b" = "hello_world")
    )
})

test_that("data.frame", {
    string <- snake(makeNames[["dataFrame"]], rownames = TRUE) %>%
        rownames() %>%
        .[[1L]]
    expect_identical(
        string,
        "mazda_rx4"
    )
})

test_that("tibble", {
    vec <- makeNames[["tibble"]] %>%
        .[, 1L:5L] %>%
        snake() %>%
        colnames()
    expect_identical(
        vec,
        c("name",
          "height",
          "mass",
          "hair_color",
          "skin_color")
    )
})

test_that("named list", {
    lst <- snake(makeNames[["list"]])
    expect_identical(
        lst,
        list("item_a" = c(1L, 2L),
             "item_b" = c(3L, 4L))
    )
})

test_that("missing", {
    message <- tryCatch(
        snake(),
        error = function(e) {
            e[["message"]]
        }
    )
    expect_identical(
        message,
        "argument \"object\" is missing, with no default"
    )
})

