context("Make Names Functions")

load("gr.rda")
load("mn.rda")

funs <- list(
    camel = camel,
    dotted = dotted,
    snake = snake,
    upperCamel = upperCamel
)



# atomic =======================================================================
with_parameters_test_that(
    "makeNames : atomic : unnamed", {
        x <- 1L
        expect_identical(f(x), x)
    },
    f = funs
)

with_parameters_test_that(
    "makeNames : atomic : named", {
        object <- c("hello.world" = 1L)
        expect_identical(
            object = names(f(object)),
            expected = expected
        )
    },
    f = funs,
    expected = c(
        "helloWorld",
        "hello.world",
        "hello_world",
        "HelloWorld"
    )
)



# character ====================================================================
test_that("makeNames : character", {
    object <- mn[["character"]]
    expected <- list(
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
            "percentGC",
            "x5prime",
            "x5.3Bias",
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
            "percent.GC",
            "X5prime",
            "X5.3.bias",
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
            "percent_gc",
            "x5prime",
            "x5_3_bias",
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
            "PercentGC",
            "X5prime",
            "X5.3Bias",
            "X123",
            NA
        )
    )
    mapply(
        fun = funs,
        expected = expected,
        MoreArgs = list(object = object),
        FUN = function(fun, object, expected) {
            expect_identical(
                object = fun(object),
                expected = expected
            )
        }
    )
})

test_that("makeNames : character (named)", {
    object <- mn[["namedCharacter"]]
    expected <- list(
        camel = c(itemA = "helloWorld", itemB = "helloWORLD"),
        dotted = c("Item.A" = "hello.world", "Item.B" = "HELLO.WORLD"),
        snake = c("item_a" = "hello_world", "item_b" = "hello_world"),
        upperCamel = c(ItemA = "HelloWorld", ItemB = "HELLOWORLD")
    )
    mapply(
        fun = funs,
        expected = expected,
        MoreArgs = list(object = object),
        FUN = function(fun, object, expected) {
            expect_identical(
                object = fun(object),
                expected = expected
            )
        }
    )
})

test_that("makeNames : character : Delimited numbers", {
    expect_identical(
        upperCamel("1,000,000"),
        "X1000000"
    )
    expect_identical(
        camel("1,000,000", strict = FALSE),
        "x1000000"
    )
    expect_identical(
        camel("1,000,000", strict = TRUE),
        "x1000000"
    )
    expect_identical(
        camel("2018-01-01", strict = FALSE),
        "x2018.01.01"
    )
    expect_identical(
        camel("2018-01-01", strict = TRUE),
        "x2018x01x01"
    )
    expect_identical(
        camel("0.01", strict = FALSE),
        "x0.01"
    )
    expect_identical(
        camel("0.01", strict = TRUE),
        "x0x01"
    )
    expect_identical(
        camel("res.0.1", strict = FALSE),
        "res0.1"
    )
    expect_identical(
        camel("res.0.1", strict = TRUE),
        "res0x1"
    )
    expect_identical(
        upperCamel("2018-01-01", strict = TRUE),
        "X2018X01X01"
    )
    expect_identical(
        upperCamel("0.01", strict = TRUE),
        "X0X01"
    )
})



# factor =======================================================================
test_that("makeNames : factor", {
    object <- mn[["factor"]]
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
    mapply(
        fun = funs,
        levels = levels,
        names = names,
        MoreArgs = list(object = object),
        FUN = function(fun, object, levels, names) {
            expect_identical(levels(fun(object)), levels)
            expect_identical(names(fun(object)), names)
        }
    )
})



# matrix / data.frame ==========================================================
test_that("makeNames : matrix", {
    object <- mn[["matrix"]][1L:3L, 1L:3L]
    expected <- list(
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
    mapply(
        fun = funs,
        expected = expected,
        MoreArgs = list(object = object),
        FUN = function(fun, object, expected) {
            expect_identical(
                dimnames(fun(object, rownames = TRUE, colnames = TRUE)),
                expected
            )
        }
    )
})



# GRanges ======================================================================
test_that("makeNames : GRanges", {
    # gr object is already camel formatted!
    expected <- list(
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
    mapply(
        fun = funs,
        expected = expected,
        MoreArgs = list(object = gr),
        FUN = function(fun, object, expected) {
            expect_identical(
                object = colnames(mcols(fun(object))),
                expected = expected
            )
        }
    )
})



# list =========================================================================
test_that("makeNames : list", {
    object <- mn[["list"]]
    expected <- list(
        camel = c("itemA", "itemB"),
        dotted = c("Item.A", "Item.B"),
        snake = c("item_a", "item_b"),
        upperCamel = c("ItemA", "ItemB")
    )
    mapply(
        fun = funs,
        expected = expected,
        MoreArgs = list(object = object),
        FUN = function(fun, object, expected) {
            expect_identical(
                object = names(fun(object)),
                expected = expected
            )
        }
    )
})



# ANY ==========================================================================
test_that("makeNames : ANY (e.g. Matrix)", {
    object <- Matrix(
        data = 1L:4L,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("gene.id.1", "gene.id.2"),
            c("sample.id.1", "sample.id.2")
        )
    )
    expected <- list(
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
    mapply(
        fun = funs,
        expected = expected,
        MoreArgs = list(object = object),
        FUN = function(fun, object, expected) {
        expect_identical(
            object = dimnames(fun(object, rownames = TRUE, colnames = TRUE)),
            expected = expected
        )
    })
})



# camel-specific ===============================================================
test_that("camel : Strict mode", {
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
            "percentGc",
            "x5prime",
            "x5x3Bias",
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
            "PercentGc",
            "X5prime",
            "X5X3Bias",
            "X123",
            NA
        )
    )
})
