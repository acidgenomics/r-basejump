context("Name Functions")

funs <- list(
    camel = camel,
    dotted = dotted,
    snake = snake,
    upperCamel = upperCamel
)



# ANY ==========================================================================
test_that("makeNames : ANY", {
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
        camel = list(
            c("geneID1", "geneID2"),
            c("sampleID1", "sampleID2")
        ),
        dotted = list(
            c("gene.ID.1", "gene.ID.2"),
            c("sample.ID.1", "sample.ID.2")
        ),
        snake = list(
            c("gene_id_1", "gene_id_2"),
            c("sample_id_1", "sample_id_2")
        ),
        upperCamel = list(
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
test_that("makeNames : character", {
    x <- mn[["character"]]
    expect <- list(
        camel = c(
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
        ),
        dotted = c(
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
        ),
        snake = c(
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
        ),
        upperCamel = c(
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
    lapply(seq_along(funs), function(i) {
        expect_identical(funs[[i]](x), expect[[i]])
    })
})

test_that("makeNames : character : named", {
    x <- mn[["namedCharacter"]]
    expect <- list(
        camel = c(itemA = "helloWorld", itemB = "helloWORLD"),
        dotted = c("Item.A" = "hello.world", "Item.B" = "HELLO.WORLD"),
        snake = c("item_a" = "hello_world", "item_b" = "hello_world"),
        upperCamel = c(ItemA = "HelloWorld", ItemB = "HELLOWORLD")
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(funs[[i]](x), expect[[i]])
    })
})

test_that("makeNames : character : camel-specific", {
    # Delimited numbers in strings
    expect_identical(camel("2018-01-01"), "x2018x01x01")
    expect_identical(camel("0.01"), "x0x01")
    expect_identical(camel("1,000,000"), "x1x000x000")
    expect_identical(upperCamel("2018-01-01"), "X2018x01x01")
    expect_identical(upperCamel("0.01"), "X0x01")
    expect_identical(upperCamel("1,000,000"), "X1x000x000")

    # Strict mode
    x <- mn[["character"]]
    expect_identical(
        camel(x, strict = TRUE),
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
    expect_identical(
        upperCamel(x, strict = TRUE),
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
})



# factor =======================================================================
test_that("makeNames : factor", {
    x <- mn[["factor"]]
    levels <- list(
        camel = c("group1", "group2"),
        dotted = c("group.1", "group.2"),
        snake = c("group_1", "group_2"),
        upperCamel = c("Group1", "Group2")
    )
    names <- list(
        camel = c("sample1", "sample2", "sample3", "sample4"),
        dotted = c("sample.1", "sample.2", "sample.3", "sample.4"),
        snake = c("sample_1", "sample_2", "sample_3", "sample_4"),
        upperCamel = c("Sample1", "Sample2", "Sample3", "Sample4")
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(levels(funs[[i]](x)), levels[[i]])
        expect_identical(names(funs[[i]](x)), names[[i]])
    })
})



# matrix / data.frame ==========================================================
test_that("makeNames : matrix", {
    x <- mn[["matrix"]][1L:3L, 1L:3L]
    expect <- list(
        camel = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urbanPop")
        ),
        dotted = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "Urban.Pop")
        ),
        snake = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urban_pop")
        ),
        upperCamel = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "UrbanPop")
        )
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(
            dimnames(funs[[i]](x, rownames = TRUE, colnames = TRUE)),
            expect[[i]]
        )
    })
})



# GRanges ======================================================================
test_that("camel : GRanges", {
    # gr object is already camel formatted!
    expect <- list(
        camel = c(
            "geneID",
            "geneName",
            "geneBiotype",
            "description",
            "seqCoordSystem",
            "entrezID",
            "broadClass"
        ),
        dotted = c(
            "gene.ID",
            "gene.Name",
            "gene.Biotype",
            "description",
            "seq.Coord.System",
            "entrez.ID",
            "broad.Class"
        ),
        snake = c(
            "gene_id",
            "gene_name",
            "gene_biotype",
            "description",
            "seq_coord_system",
            "entrez_id",
            "broad_class"
        ),
        upperCamel = c(
            "GeneID",
            "GeneName",
            "GeneBiotype",
            "Description",
            "SeqCoordSystem",
            "EntrezID",
            "BroadClass"
        )
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(
            colnames(mcols(funs[[i]](gr))),
            expect[[i]]
        )
    })
})



# list =========================================================================
test_that("camel : list", {
    x <- mn[["list"]]
    expect <- list(
        camel = c("itemA", "itemB"),
        dotted = c("Item.A", "Item.B"),
        snake = c("item_a", "item_b"),
        upperCamel = c("ItemA", "ItemB")
    )
    lapply(seq_along(funs), function(i) {
        expect_identical(names(funs[[i]](x)), expect[[i]])
    })
})
