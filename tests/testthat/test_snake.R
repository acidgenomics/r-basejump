context("snake")

loadRemoteData("http://basejump.seq.cloud/makeNames.rda", quiet = TRUE)

test_that("character", {
    vec <- snake(makeNames[["vec"]])
    expect_equal(
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
    vec <- snake(makeNames[["namedVec"]])
    expect_equal(
        vec,
        c(item_a = "hello world",
          item_b = "HELLO WORLD")
    )
})

test_that("data.frame", {
    string <- snake(makeNames[["df"]], rownames = TRUE) %>%
        rownames() %>%
        .[[1L]]
    expect_equal(
        string,
        "mazda_rx4"
    )
})

test_that("tibble", {
    vec <- makeNames[["tbl"]] %>%
        .[, 1L:5L] %>%
        snake() %>%
        colnames()
    expect_equal(
        vec,
        c("name",
          "height",
          "mass",
          "hair_color",
          "skin_color")
    )
})

test_that("named list", {
    lst <- snake(makeNames[["lst"]])
    expect_equal(
        lst,
        list(item_a = c(1L, 2L),
             item_b = c(3L, 4L))
    )
})

test_that("missing", {
    message <- tryCatch(
        snake(),
        error = function(e) {
            e[["message"]]
        }
    )
    expect_equal(
        message,
        "argument \"object\" is missing, with no default"
    )
})
