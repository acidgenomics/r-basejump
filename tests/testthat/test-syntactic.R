data(syntactic, package = "acidtest", envir = environment())
funs <- list(
    camelCase = camelCase,
    dottedCase = dottedCase,
    snakeCase = snakeCase,
    upperCamelCase = upperCamelCase
)



context("syntactic : atomic")

with_parameters_test_that(
    "Unnamed", {
        object <- 1L
        expect_identical(f(object), object)
    },
    f = funs
)

with_parameters_test_that(
    "Named", {
        object <- c("hello.world" = 1L)
        expect_identical(names(f(object)), expected)
    },
    f = funs,
    expected = c(
        "helloWorld",
        "hello.world",
        "hello_world",
        "HelloWorld"
    )
)



context("syntactic : character")

## Don't attempt to test kebabCase here, it doesn't support names argument.
with_parameters_test_that(
    "Named", {
        object <- syntactic[["character_named"]]
        expect_identical(
            object = f(object, names = TRUE),
            expected = expected
        )
        expect_identical(
            object = names(f(object, names = FALSE)),
            expected = names(object)
        )
    },
    f = funs,
    expected = list(
        camelCase = c(
            itemA = "helloWorld",
            itemB = "helloWORLD"
        ),
        dottedCase = c(
            "Item.A" = "hello.world",
            "Item.B" = "HELLO.WORLD"
        ),
        snakeCase = c(
            "item_a" = "hello_world",
            "item_b" = "hello_world"
        ),
        upperCamelCase = c(
            ItemA = "HelloWorld",
            ItemB = "HELLOWORLD"
        )
    )
)

with_parameters_test_that(
    "Unnamed", {
        object <- syntactic[["character"]]
        expect_identical(
            object = f(object),
            expected = expected
        )
    },
    f = c(funs, kebabCase = kebabCase),
    expected = list(
        camelCase = c(
            "percentGC",
            "x10um",
            "x5x3Bias",
            "x5prime",
            "g2mScore",
            "helloWorld",
            "helloWORLD",
            "mazdaRX4",
            "nCount",
            "rnaiClones",
            "tx2gene",
            "tx2GeneID",
            "worfdbHTMLRemap",
            "x123",
            NA
        ),
        dottedCase = c(
            "percent.GC",
            "X10um",
            "X5.3.bias",
            "X5prime",
            "G2M.Score",
            "hello.world",
            "HELLO.WORLD",
            "Mazda.RX4",
            "n.Count",
            "RNAI.clones",
            "tx2gene",
            "TX2.Gene.ID",
            "worfdb.HTML.Remap",
            "X123",
            NA
        ),
        snakeCase = c(
            "percent_gc",
            "x10um",
            "x5_3_bias",
            "x5prime",
            "g2m_score",
            "hello_world",
            "hello_world",
            "mazda_rx4",
            "n_count",
            "rnai_clones",
            "tx2gene",
            "tx2_gene_id",
            "worfdb_html_remap",
            "x123",
            NA
        ),
        upperCamelCase = c(
            "PercentGC",
            "X10um",
            "X5X3Bias",
            "X5prime",
            "G2MScore",
            "HelloWorld",
            "HELLOWORLD",
            "MazdaRX4",
            "NCount",
            "RNAIClones",
            "Tx2gene",
            "TX2GeneID",
            "WorfdbHTMLRemap",
            "X123",
            NA
        ),
        kebabCase = c(
            "percent-gc",
            "x10um",
            "x5-3-bias",
            "x5prime",
            "g2m-score",
            "hello-world",
            "hello-world",
            "mazda-rx4",
            "n-count",
            "rnai-clones",
            "tx2gene",
            "tx2-gene-id",
            "worfdb-html-remap",
            "x123",
            NA
        )
    )
)

with_parameters_test_that(
    "camelCase of delimited numbers", {
        object <- c(
            "1,000,000",
            "0.01",
            "2018-01-01",
            "res.0.1"
        )
        expect_identical(
            object = f(object, strict = strict),
            expected = expected
        )
    },
    f = list(
        camelCase,
        camelCase,
        upperCamelCase
    ),
    strict = list(
        FALSE,
        TRUE,
        TRUE
    ),
    expected = list(
        camelCase_normal = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        ),
        camelCase_strict = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        ),
        upperCamelCase_strict = c(
            "X1000000",
            "X0X01",
            "X2018X01X01",
            "Res0X1"
        )
    )
)

with_parameters_test_that(
    "Plus minus handling", {
        object <- c(
            "100%",
            "+/-",
            "a +/- b",
            "dox-",
            "dox+",
            "-dox",
            "+dox",
            "/",
            "-"
        )
        expect_identical(
            object = fun(object),
            expected = expected
        )
    },
    fun = list(camelCase, snakeCase),
    expected = list(
        camelCase = c(
            "x100Percent",
            "plusSlashMinus",
            "aPlusSlashMinusB",
            "doxMinus",
            "doxPlus",
            "xDox",
            "plusDox",
            "slash",
            "x"
        ),
        snakeCase = c(
            "x100_percent",
            "plus_slash_minus",
            "a_plus_slash_minus_b",
            "dox_minus",
            "dox_plus",
            "x_dox",
            "plus_dox",
            "slash",
            "x"
        )
    )
)



context("syntactic : factor")

with_parameters_test_that(
    "factor", {
        object <- syntactic[["factor"]]
        x <- f(object, names = TRUE)
        expect_identical(
            object = levels(x),
            expected = levels
        )
        expect_identical(
            object = names(x),
            expected = names
        )
        x <- f(object, names = FALSE)
        expect_identical(
            object = names(x),
            expected = names(object)
        )
    },
    f = funs,
    levels = list(
        camelCase = c("group1", "group2"),
        dottedCase = c("group.1", "group.2"),
        snakeCase = c("group_1", "group_2"),
        upperCamelCase = c("Group1", "Group2")
    ),
    names = list(
        camelCase = c("sample1", "sample2", "sample3", "sample4"),
        dottedCase = c("sample.1", "sample.2", "sample.3", "sample.4"),
        snakeCase = c("sample_1", "sample_2", "sample_3", "sample_4"),
        upperCamelCase = c("Sample1", "Sample2", "Sample3", "Sample4")
    )
)



context("syntactic : list")

with_parameters_test_that(
    "list", {
        object <- syntactic[["list"]]
        expect_identical(
            object = names(f(object)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camel = c("itemA", "itemB"),
        dotted = c("Item.A", "Item.B"),
        snake = c("item_a", "item_b"),
        upperCamel = c("ItemA", "ItemB")
    )
)



context("syntactic : matrix")

with_parameters_test_that(
    "matrix", {
        object <- syntactic[["matrix"]][seq_len(3L), seq_len(3L)]
        expect_identical(
            object = dimnames(f(object, rownames = TRUE, colnames = TRUE)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urbanPop")
        ),
        dottedCase = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "Urban.Pop")
        ),
        snakeCase = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urban_pop")
        ),
        upperCamelCase = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "UrbanPop")
        )
    )
)



context("syntactic : Vector")

object <- as(df, "Vector")
names(object) <- toupper(names(object))
mcols(object) <- DataFrame(TEST = seq_len(length(object)))
metadata(object) <- list(TEST = "XXX")

with_parameters_test_that(
    "SimpleList", {
        x <- f(
            object = object,
            names = TRUE,
            mcols = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "SimpleList")
    },
    f = funs
)



context("syntactic : DataTable")

mcols(df) <- DataFrame(TEST = seq_len(ncol(df)))
metadata(df) <- list(TEST = "XXX")

with_parameters_test_that(
    "DataFrame", {
        x <- f(
            object = df,
            rownames = TRUE,
            colnames = TRUE,
            mcols = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "DataFrame")
    },
    f = funs
)



context("syntactic : GRanges")

with_parameters_test_that(
    "GRanges", {
        x <- f(
            object = gr,
            names = TRUE,
            mcols = TRUE
        )
        expect_s4_class(x, "GRanges")
        expect_identical(
            object = colnames(mcols(x)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = c("geneID", "geneName"),
        dottedCase = c("gene.ID", "gene.Name"),
        snakeCase = c("gene_id", "gene_name"),
        upperCamelCase = c("GeneID", "GeneName")
    )
)



context("syntactic : SummarizedExperiment")

with_parameters_test_that(
    "SummarizedExperiment", {
        x <- f(
            object = rse,
            rownames = TRUE,
            colnames = TRUE,
            assayNames = TRUE,
            rowData = TRUE,
            colData = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "SummarizedExperiment")
    },
    f = funs
)
