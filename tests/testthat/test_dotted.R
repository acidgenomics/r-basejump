context("dotted")

load(system.file("extdata/makeNames.rda", package = "basejump"))

test_that("ANY", {
    # Integer (atomic)
    expect_identical(dotted(1L), 1L)
    expect_identical(
        dotted(c("helloWorld" = 1L)),
        c("hello.World" = 1L)
    )

    # Matrix (dimnames)
    data <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene_id_1", "gene_id_2"),
            c("sample_id_1", "sample_id_2")
        )
    )
    expect_identical(
        dimnames(dotted(data, rownames = TRUE, colnames = TRUE)),
        list(
            c("gene.ID.1", "gene.ID.2"),
            c("sample.ID.1", "sample.ID.2")
        )
    )
})

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
