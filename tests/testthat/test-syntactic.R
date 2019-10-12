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
