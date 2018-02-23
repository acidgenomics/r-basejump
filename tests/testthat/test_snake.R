context("snake")

test_that("ANY", {
    # Integer (atomic)
    expect_identical(snake(1L), 1L)
    expect_identical(
        snake(c("hello.world" = 1L)),
        c("hello_world" = 1L)
    )

    # Matrix (dimnames)
    data <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene.id.1", "gene.id.2"),
            c("sample.id.1", "sample.id.2")
        )
    )
    expect_identical(
        dimnames(snake(data, rownames = TRUE, colnames = TRUE)),
        list(
            c("gene_id_1", "gene_id_2"),
            c("sample_id_1", "sample_id_2")
        )
    )
})

test_that("Character", {
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

test_that("Named character", {
    vec <- snake(makeNames[["namedCharacter"]])
    expect_identical(
        vec,
        c("item_a" = "hello_world",
          "item_b" = "hello_world")
    )
})

test_that("Data frame", {
    string <- snake(makeNames[["dataFrame"]], rownames = TRUE) %>%
        rownames() %>%
        .[[1L]]
    expect_identical(
        string,
        "mazda_rx4"
    )
})

test_that("Counts matrix", {
    counts <- snake(counts)
    expect_identical(
        rownames(counts)[[1L]],
        "ENSMUSG00000002459"
    )
    expect_identical(
        colnames(counts),
        c("group1_1", "group1_2", "group2_1", "group2_2")
    )
})

test_that("Matrix rownames", {
    # Sanitize rownames
    expect_identical(
        snake(makeNames[["matrix"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazda_rx4"
    )
    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        makeNames[["matrix"]] %>%
            set_rownames(NULL) %>%
            snake(rownames = TRUE) %>%
            rownames(),
        NULL
    )
})

test_that("Tibble", {
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

test_that("Named list", {
    lst <- snake(makeNames[["list"]])
    expect_identical(
        lst,
        list("item_a" = c(1L, 2L),
             "item_b" = c(3L, 4L))
    )
})

test_that("Missing", {
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
