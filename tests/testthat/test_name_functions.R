context("Name Functions")

funs <- list(
    "camel" = camel,
    "dotted" = dotted,
    "snake" = snake,
    "upperCamel" = upperCamel
)



# ANY ==========================================================================
test_that("ANY", {
    # Atomic
    x <- 1L
    lapply(funs, function(fun) {
        expect_identical(fun(x), x)
    })

    # Named atomic
    x <- c("hello.world" = 1L)
    expect <- c(
        "helloWorld",
        "hello.world",
        "hello_world",
        "HelloWorld"
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(names(funs[[i]](x)), expect[[i]])
    })

    # Sparse matrix (dimnames)
    x <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene.id.1", "gene.id.2"),
            c("sample.id.1", "sample.id.2")
        )
    )
    expect <- list(
        "camel" = list(
            c("geneID1", "geneID2"),
            c("sampleID1", "sampleID2")
        ),
        "dotted" = list(
            c("gene.ID.1", "gene.ID.2"),
            c("sample.ID.1", "sample.ID.2")
        ),
        "snake" = list(
            c("gene_id_1", "gene_id_2"),
            c("sample_id_1", "sample_id_2")
        ),
        "upperCamel" = list(
            c("GeneID1", "GeneID2"),
            c("SampleID1", "SampleID2")
        )
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(
            dimnames(funs[[i]](x, rownames = TRUE, colnames = TRUE)),
            expect[[i]]
        )
    })
})



# character ====================================================================
test_that("camel : character", {
    expect_identical(
        camel(mn[["character"]], strict = FALSE),
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
        camel(mn[["character"]], strict = TRUE),
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
        camel(mn[["namedCharacter"]], strict = TRUE),
        c("itemA" = "helloWorld", "itemB" = "helloWorld")
    )
})

test_that("camel : data.frame", {
    # Sanitize rownames
    x <- camel(mn[["dataFrame"]], rownames = TRUE, strict = TRUE)
    expect_identical(rownames(x)[[1L]], "alabama")
    # Unset rownames should be skipped, even when `rownames = TRUE`
    expect_identical(
        mn[["dataFrame"]] %>%
            set_rownames(NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(mn[["dataFrame"]]))
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
        camel(mn[["list"]], strict = TRUE),
        list("itemA" = c(1L, 2L), "itemB" = c(3L, 4L))
    )
})

test_that("camel : matrix", {
    x <- camel(mn[["matrix"]])
    expect_identical(
        rownames(x)[[1L]],
        "gene_1"
    )
    expect_identical(
        colnames(x),
        c("sample1x1", "sample1x2", "sample2x1", "sample2x2")
    )

    # Sanitize rownames
    expect_identical(
        camel(mn[["matrix"]], rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazdaRx4"
    )

    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        mn[["matrix"]] %>%
            set_rownames(NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        NULL
    )
})



# dotted =======================================================================
test_that("dotted : character", {
    expect_identical(
        dotted(mn[["character"]]),
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
        dotted(mn[["namedCharacter"]]),
        c("Item.A" = "hello.world", "Item.B" = "HELLO.WORLD")
    )
})

test_that("dotted : data.frame", {
    expect_identical(
        dotted(mn[["dataFrame"]], rownames = TRUE) %>%
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
        dotted(mn[["list"]]),
        list("Item.A" = c(1L, 2L), "Item.B" = c(3L, 4L))
    )
})

test_that("dotted : matrix", {
    x <- dotted(mn[["matrix"]])
    expect_identical(
        rownames(x)[[1L]],
        "gene_1"
    )
    expect_identical(
        colnames(x),
        c("sample.1.1", "sample.1.2", "sample.2.1", "sample.2.2")
    )

    # Sanitize rownames
    expect_identical(
        dotted(mn[["matrix"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "Mazda.RX4"
    )

    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        mn[["matrix"]] %>%
            set_rownames(NULL) %>%
            dotted(rownames = TRUE) %>%
            rownames(),
        NULL
    )
})



# snake ========================================================================
test_that("snake : character", {
    x <- snake(mn[["character"]])
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
    x <- snake(mn[["namedCharacter"]])
    expect_identical(
        x,
        c("item_a" = "hello_world", "item_b" = "hello_world")
    )
})

test_that("snake : data.frame", {
    x <- snake(mn[["dataFrame"]], rownames = TRUE) %>%
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
    x <- snake(mn[["list"]])
    expect_identical(
        x,
        list("item_a" = c(1L, 2L), "item_b" = c(3L, 4L))
    )
})

test_that("snake : matrix", {
    # Already formatted in snake case
    x <- snake(mn[["matrix"]])
    expect_identical(
        rownames(x)[[1L]],
        rownames(mn[["matrix"]])[[1L]]
    )
    expect_identical(
        colnames(x),
        colnames(mn[["matrix"]])
    )

    # Sanitize rownames
    expect_identical(
        snake(mn[["matrix"]], rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazda_rx4"
    )

    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        mn[["matrix"]] %>%
            set_rownames(NULL) %>%
            snake(rownames = TRUE) %>%
            rownames(),
        NULL
    )
})



# upperCamel ===================================================================
test_that("upperCamel : character", {
    expect_identical(
        upperCamel(mn[["character"]], strict = FALSE),
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
        upperCamel(mn[["character"]], strict = TRUE),
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
        upperCamel(mn[["namedCharacter"]], strict = TRUE),
        c("ItemA" = "HelloWorld", "ItemB" = "HelloWorld")
    )
})

test_that("upperCamel : data.frame", {
    # Sanitize rownames
    expect_identical(
        upperCamel(mn[["dataFrame"]],
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
        upperCamel(mn[["list"]], strict = TRUE),
        list("ItemA" = c(1L, 2L), "ItemB" = c(3L, 4L))
    )
})

test_that("upperCamel : matrix", {
    x <- upperCamel(mn[["matrix"]])
    expect_identical(
        rownames(x)[[1L]],
        "gene_1"
    )
    expect_identical(
        colnames(x),
        c("Sample1x1", "Sample1x2", "Sample2x1", "Sample2x2")
    )

    # Sanitize rownames
    expect_identical(
        upperCamel(
            mn[["matrix"]],
            rownames = TRUE, strict = TRUE
        ) %>%
            rownames() %>%
            .[[1L]],
        "MazdaRx4"
    )

    # Unset rownames (ignore in `.checkRownames()`)
    expect_identical(
        mn[["matrix"]] %>%
            set_rownames(NULL) %>%
            upperCamel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        NULL
    )
})
