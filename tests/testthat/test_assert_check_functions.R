context("Assert Checks")

test_that("assertAllAreNonExisting", {
    expect_silent(assertAllAreNonExisting(c("a", "b", "c")))
    a <- 1L
    b <- 2L
    expect_error(assertAllAreNonExisting(c("a", "b", "c")))
})

test_that("assertFormalAnnotationCol", {
    x <- df
    y <- as.data.frame(coldata)
    expect_silent(assertFormalAnnotationCol(x, y))
    expect_silent(assertFormalAnnotationCol(x, NA))
    expect_silent(assertFormalAnnotationCol(x, NULL))
    expect_error(assertFormalAnnotationCol(x, coldata))
    expect_error(assertFormalAnnotationCol(mtcars, y))
})

test_that("assertFormalCompress", {
    expect_error(
        assertFormalCompress("XXX"),
        paste(
            "is_subset :",
            "The element 'XXX' in x is not in",
            "c\\(\"bzip2\", \"gzip\", \"xz\"\\)."
        )
    )
    expect_silent(assertFormalCompress("xz"))
})

test_that("assertFormalGene2symbol", {
    gene2symbol <- gene2symbol(human, release = ensemblRelease)
    genes <- head(rownames(gene2symbol), 2L)
    x <- data.frame(
        sample1 = c(1L, 2L),
        saple2 = c(3L, 4L),
        row.names = genes,
        stringsAsFactors = FALSE
    )
    expect_silent(assertFormalGene2symbol(x, genes, gene2symbol))
    expect_error(
        assertFormalGene2symbol(mtcars, genes, gene2symbol),
        paste(
            "is_subset :",
            "The elements 'ENSG00000000003', 'ENSG00000000005'",
            "in genes are not in rownames\\(x\\)."
        )
    )
})

test_that("assertHasRownames", {
    expect_silent(assertHasRownames(df))
    rownames(df) <- NULL
    expect_error(assertHasRownames(df))
    expect_error(assertHasRownames(tbl))
})

test_that("assertIsDataFrameOrNULL", {
    expect_silent(assertIsDataFrameOrNULL(mtcars))
    expect_silent(assertIsDataFrameOrNULL(NULL))
    expect_error(
        assertIsDataFrameOrNULL(1L),
        "is2 : x is not in any of the classes 'data.frame', 'NULL'."
    )
})

test_that("assertIsAHeaderLevel", {
    expect_silent(assertIsAHeaderLevel(1L))
    expect_error(
        assertIsAHeaderLevel(8L),
        paste(
            "is_subset :",
            "The element '8' in as.integer\\(x\\) is not in seq\\(1L:7L\\)."
        )
    )
})

test_that("assertIsAnImplicitInteger", {
    expect_silent(assertIsAnImplicitInteger(1))
    expect_silent(assertIsAnImplicitInteger(1L))
    expect_silent(assertIsAnImplicitInteger(1.0))
    expect_error(assertIsAnImplicitInteger(c(1L, 2L)))
    expect_error(assertIsAnImplicitInteger(1.1))
    # Tolerance threshold
    expect_error(assertIsImplicitInteger(1.000000000000001))
})

test_that("assertIsAnImplicitIntegerOrNULL", {
    expect_silent(assertIsAnImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1L))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1.0))
    expect_error(assertIsAnImplicitIntegerOrNULL(1.1))
})

test_that("assertIsAnIntegerOrNULL", {
    expect_silent(assertIsAnIntegerOrNULL(1L))
    expect_silent(assertIsAnIntegerOrNULL(NULL))
    expect_error(
        assertIsAnIntegerOrNULL(c(1L, 2L)),
        "is_an_integer : x has length 2, not 1."
    )
})

test_that("assertAreGeneAnnotations", {
    x <- genes(human)
    expect_silent(assertAreGeneAnnotations(x))
    expect_error(
        assertAreGeneAnnotations(mtcars),
        "is_subset : "
    )
})

test_that("assertAreTranscriptAnnotations", {
    x <- transcripts(human)
    expect_silent(assertAreTranscriptAnnotations(x))
    expect_error(
        assertAreTranscriptAnnotations(mtcars),
        "is_subset : "
    )
})

test_that("assertIsANumberOrNULL", {
    expect_silent(assertIsANumberOrNULL(1.1))
    expect_silent(assertIsANumberOrNULL(NULL))
    expect_error(
        assertIsANumberOrNULL(c(1.1, 1.2)),
        "is_a_number : x has length 2, not 1."
    )
})

test_that("assertIsAStringOrNULL", {
    expect_silent(assertIsAStringOrNULL("hello world"))
    expect_silent(assertIsAStringOrNULL(NULL))
    expect_error(
        assertIsAStringOrNULL(c("hello", "world")),
        "is_a_string : x has length 2, not 1."
    )
})

test_that("assertIsCharacterOrNULL", {
    expect_silent(assertIsCharacterOrNULL(c("hello", "world")))
    expect_silent(assertIsCharacterOrNULL(NULL))
    expect_error(
        assertIsCharacterOrNULL(1L),
        "is2 : x is not in any of the classes 'character', 'NULL'."
    )
})

test_that("assertIsColorScaleContinuousOrNULL", {
    color <- scale_color_viridis(discrete = FALSE)
    expect_silent(assertIsColorScaleContinuousOrNULL(color))
    expect_silent(assertIsColorScaleContinuousOrNULL(NULL))
    color <- scale_color_viridis(discrete = TRUE)
    expect_error(
        assertIsColorScaleContinuousOrNULL(color),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})

test_that("assertIsColorScaleDiscreteOrNULL", {
    color <- scale_color_viridis(discrete = TRUE)
    expect_silent(assertIsColorScaleDiscreteOrNULL(color))
    expect_silent(assertIsColorScaleDiscreteOrNULL(NULL))
    color <- scale_color_viridis(discrete = FALSE)
    expect_error(
        assertIsColorScaleDiscreteOrNULL(color),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})

test_that("assertIsFillScaleContinuousOrNULL", {
    fill <- scale_fill_viridis(discrete = FALSE)
    expect_silent(assertIsFillScaleContinuousOrNULL(fill))
    expect_silent(assertIsFillScaleContinuousOrNULL(NULL))
    fill <- scale_fill_viridis(discrete = TRUE)
    expect_error(
        assertIsFillScaleContinuousOrNULL(fill),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})

test_that("assertIsFillScaleDiscreteOrNULL", {
    fill <- scale_fill_viridis(discrete = TRUE)
    expect_silent(assertIsFillScaleDiscreteOrNULL(fill))
    expect_silent(assertIsFillScaleDiscreteOrNULL(NULL))
    fill <- scale_fill_viridis(discrete = FALSE)
    expect_error(
        assertIsFillScaleDiscreteOrNULL(fill),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})

test_that("assertIsGene2symbol", {
    gene2symbol <- gene2symbol(human)
    expect_silent(assertIsGene2symbol(gene2symbol))
    expect_error(
        assertIsGene2symbol(mtcars),
        paste(
            "are_identical :",
            "colnames\\(x\\) and c\\(\"ensgene\", \"symbol\"\\)",
            "are not identical."
        )
    )
    expect_error(
        assertIsGene2symbol(NULL),
        "is_data.frame : x"
    )
})

test_that("assertIsHexColorFunctionOrNULL", {
    expect_silent(assertIsHexColorFunctionOrNULL(viridis))
    expect_error(assertIsHexColorFunctionOrNULL(viridis(256L)))
})

test_that("assertIsImplicitInteger", {
    expect_silent(assertIsImplicitInteger(c(1, 2)))  # nolint
    expect_silent(assertIsImplicitInteger(c(1L, 2L)))
    expect_silent(assertIsImplicitInteger(c(1.0, 2.0)))
    expect_error(assertIsImplicitInteger(c(1.1, 2.1)))
})

test_that("assertIsImplicitIntegerOrNULL", {
    expect_silent(assertIsImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1, 2)))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1L, 2L)))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1.0, 2.0)))
    expect_error(assertIsImplicitIntegerOrNULL(c(1.1, 2.1)))
})

test_that("isImplicitInteger", {
    expect_true(isImplicitInteger(1))
    expect_true(isImplicitInteger(1L))
    expect_true(isImplicitInteger(1.0))
    expect_false(isImplicitInteger(1.1))
})

test_that("assertIsTx2gene", {
    tx2gene <- tx2gene(human)
    expect_silent(assertIsTx2gene(tx2gene))
    expect_error(
        assertIsTx2gene(mtcars),
        paste(
            "are_identical :",
            "colnames\\(x\\) and c\\(\"enstxp\", \"ensgene\"\\)",
            "are not identical."
        )
    )
    expect_error(
        assertIsTx2gene(NULL),
        "is_data.frame : x"
    )
})
