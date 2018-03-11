context("Name Functions")

# camel ========================================================================
test_that("camel : ANY", {
    # Integer (atomic)
    expect_identical(camel(1L), 1L)
    expect_identical(
        camel(c("hello.world" = 1L)),
        c("helloWorld" = 1L)
    )

    # Matrix (dimnames)
    x <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene.id.1", "gene.id.2"),
            c("sample.id.1", "sample.id.2")
        )
    )
    expect_identical(
        dimnames(camel(x, rownames = TRUE, colnames = TRUE)),
        list(
            c("geneID1", "geneID2"),
            c("sampleID1", "sampleID2")
        )
    )
})

test_that("camel : character", {
    expect_identical(
        camel(makeNames[["character"]], strict = FALSE),
        c(
            "helloWorld",
            "helloWORLD",
            "rnaiClones",
            "nCount",
            "tx2gene",
            "tx2GeneID",
            "g2mScore",
            "worfdbHTMLRemap",
            "mazdaRX4",
            "x123",
            NA
        )
    )
    expect_identical(
        camel(makeNames[["character"]], strict = TRUE),
        c(
            "helloWorld",
            "helloWorld",
            "rnaiClones",
            "nCount",
            "tx2gene",
            "tx2GeneId",
            "g2mScore",
            "worfdbHtmlRemap",
            "mazdaRx4",
            "x123",
            NA
        )
    )

    # Delimited numbers in strings
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

    # Named
    expect_identical(
        camel(makeNames[["namedCharacter"]], strict = TRUE),
        c("itemA" = "helloWorld", "itemB" = "helloWorld")
    )
})

test_that("camel : data.frame", {
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

test_that("camel : GRanges", {
    # gr object is already camel formatted!
    colnames <- colnames(mcols(gr))
    x <- snake(gr)
    expect_identical(
        camel(x) %>%
            mcols() %>%
            colnames(),
        colnames
    )
})

test_that("camel : list", {
    expect_identical(
        camel(makeNames[["list"]], strict = TRUE),
        list("itemA" = c(1L, 2L), "itemB" = c(3L, 4L))
    )
})

test_that("camel : matrix", {
    x <- camel(mat)
    expect_identical(
        rownames(x)[[1L]],
        "ENSG00000000001"
    )
    expect_identical(
        colnames(x),
        c("group1x1", "group1x2", "group2x1", "group2x2")
    )

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

test_that("camel : tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            camel(strict = TRUE) %>%
            colnames(),
        c("name", "height", "mass", "hairColor", "skinColor")
    )
})



# dotted =======================================================================
test_that("dotted : ANY", {
    # Integer (atomic)
    expect_identical(dotted(1L), 1L)
    expect_identical(
        dotted(c("helloWorld" = 1L)),
        c("hello.World" = 1L)
    )

    # Matrix (dimnames)
    x <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene_id_1", "gene_id_2"),
            c("sample_id_1", "sample_id_2")
        )
    )
    expect_identical(
        dimnames(dotted(x, rownames = TRUE, colnames = TRUE)),
        list(
            c("gene.ID.1", "gene.ID.2"),
            c("sample.ID.1", "sample.ID.2")
        )
    )
})

test_that("dotted : character", {
    expect_identical(
        dotted(makeNames[["character"]]),
        c(
            "hello.world",
            "HELLO.WORLD",
            "RNAI.clones",
            "n.Count",
            "tx2gene",
            "TX2.Gene.ID",
            "G2M.Score",
            "worfdb.HTML.Remap",
            "Mazda.RX4",
            "X123",
            NA
        )
    )

    # Named
    expect_identical(
        dotted(makeNames[["namedCharacter"]]),
        c("Item.A" = "hello.world", "Item.B" = "HELLO.WORLD")
    )
})

test_that("dotted : data.frame", {
    expect_identical(
        dotted(makeNames[["dataFrame"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "Mazda.RX4"
    )
})

test_that("dotted : GRanges", {
    x <- dotted(gr)
    expect_identical(
        colnames(mcols(x)),
        c(
            "gene.ID",
            "gene.Name",
            "gene.Biotype",
            "description",
            "seq.Coord.System",
            "entrez.ID",
            "broad.Class"
        )
    )
})

test_that("dotted : list", {
    expect_identical(
        dotted(makeNames[["list"]]),
        list("Item.A" = c(1L, 2L), "Item.B" = c(3L, 4L))
    )
})

test_that("dotted : matrix", {
    x <- dotted(mat)
    expect_identical(
        rownames(x)[[1L]],
        "ENSG00000000001"
    )
    expect_identical(
        colnames(x),
        c("group1.1", "group1.2", "group2.1", "group2.2")
    )

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

test_that("dotted : tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            dotted() %>%
            colnames(),
        c(
            "name",
            "height",
            "mass",
            "hair.color",
            "skin.color"
        )
    )
})



# snake ========================================================================
test_that("snake : ANY", {
    # Integer (atomic)
    expect_identical(snake(1L), 1L)
    expect_identical(
        snake(c("hello.world" = 1L)),
        c("hello_world" = 1L)
    )

    # Matrix (dimnames)
    x <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene.id.1", "gene.id.2"),
            c("sample.id.1", "sample.id.2")
        )
    )
    expect_identical(
        dimnames(snake(x, rownames = TRUE, colnames = TRUE)),
        list(
            c("gene_id_1", "gene_id_2"),
            c("sample_id_1", "sample_id_2")
        )
    )
})

test_that("snake : character", {
    x <- snake(makeNames[["character"]])
    expect_identical(
        x,
        c(
            "hello_world",
            "hello_world",
            "rnai_clones",
            "n_count",
            "tx2gene",
            "tx2_gene_id",
            "g2m_score",
            "worfdb_html_remap",
            "mazda_rx4",
            "x123",
            NA
        )
    )

    # Named
    x <- snake(makeNames[["namedCharacter"]])
    expect_identical(
        x,
        c("item_a" = "hello_world", "item_b" = "hello_world")
    )
})

test_that("snake : data.frame", {
    x <- snake(makeNames[["dataFrame"]], rownames = TRUE) %>%
        rownames() %>%
        .[[1L]]
    expect_identical(
        x,
        "mazda_rx4"
    )
})

test_that("snake : GRanges", {
    x <- snake(gr)
    expect_identical(
        colnames(mcols(x)),
        c(
            "gene_id",
            "gene_name",
            "gene_biotype",
            "description",
            "seq_coord_system",
            "entrez_id",
            "broad_class"
        )
    )
})

test_that("snake : list", {
    x <- snake(makeNames[["list"]])
    expect_identical(
        x,
        list("item_a" = c(1L, 2L), "item_b" = c(3L, 4L))
    )
})

test_that("snake : matrix", {
    x <- snake(mat)
    expect_identical(
        rownames(x)[[1L]],
        "ENSG00000000001"
    )
    expect_identical(
        colnames(x),
        c("group1_1", "group1_2", "group2_1", "group2_2")
    )

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

test_that("snake : tibble", {
    x <- makeNames[["tibble"]] %>%
        .[, 1L:5L] %>%
        snake() %>%
        colnames()
    expect_identical(
        x,
        c(
            "name",
            "height",
            "mass",
            "hair_color",
            "skin_color"
        )
    )
})



# upperCamel ===================================================================
test_that("upperCamel : ANY", {
    # Integer (atomic)
    expect_identical(upperCamel(1L), 1L)
    expect_identical(
        upperCamel(c("hello.world" = 1L)),
        c("HelloWorld" = 1L)
    )

    # Matrix (dimnames)
    x <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene.id.1", "gene.id.2"),
            c("sample.id.1", "sample.id.2")
        )
    )
    expect_identical(
        dimnames(upperCamel(x, rownames = TRUE, colnames = TRUE)),
        list(
            c("GeneID1", "GeneID2"),
            c("SampleID1", "SampleID2")
        )
    )
})

test_that("upperCamel : character", {
    expect_identical(
        upperCamel(makeNames[["character"]], strict = FALSE),
        c(
            "HelloWorld",
            "HELLOWORLD",  # improve this?
            "RNAIClones",
            "NCount",
            "Tx2gene",
            "TX2GeneID",
            "G2MScore",
            "WorfdbHTMLRemap",
            "MazdaRX4",
            "X123",
            NA
        )
    )
    expect_identical(
        upperCamel(makeNames[["character"]], strict = TRUE),
        c(
            "HelloWorld",
            "HelloWorld",
            "RnaiClones",
            "NCount",
            "Tx2gene",
            "Tx2GeneId",
            "G2mScore",
            "WorfdbHtmlRemap",
            "MazdaRx4",
            "X123",
            NA
        )
    )

    # Delimited numbers as strings
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

    # Named
    expect_identical(
        upperCamel(makeNames[["namedCharacter"]], strict = TRUE),
        c("ItemA" = "HelloWorld", "ItemB" = "HelloWorld")
    )
})

test_that("upperCamel : data.frame", {
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

test_that("upperCamel : GRanges", {
    x <- upperCamel(gr)
    expect_identical(
        colnames(mcols(x)),
        c(
            "GeneID",
            "GeneName",
            "GeneBiotype",
            "Description",
            "SeqCoordSystem",
            "EntrezID",
            "BroadClass"
        )
    )
})

test_that("upperCamel : list", {
    expect_identical(
        upperCamel(makeNames[["list"]], strict = TRUE),
        list("ItemA" = c(1L, 2L), "ItemB" = c(3L, 4L))
    )
})

test_that("upperCamel : matrix", {
    x <- upperCamel(mat)
    expect_identical(
        rownames(x)[[1L]],
        "ENSG00000000001"
    )
    expect_identical(
        colnames(x),
        c("Group1x1", "Group1x2", "Group2x1", "Group2x2")
    )

    # Sanitize rownames
    expect_identical(
        upperCamel(
            makeNames[["matrix"]],
            rownames = TRUE, strict = TRUE
        ) %>%
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

test_that("upperCamel : tibble", {
    expect_identical(
        makeNames[["tibble"]] %>%
            .[, 1L:5L] %>%
            upperCamel(strict = TRUE) %>%
            colnames(),
        c("Name", "Height", "Mass", "HairColor", "SkinColor")
    )
})
