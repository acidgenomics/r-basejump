context("Assert Check Functions")

g2s <- tibble(
    geneID = c("gene_1", "gene_2"),
    geneName = c("symbol_1", "symbol_2")
)



test_that("assertAllAreNonExisting", {
    expect_silent(assertAllAreNonExisting(c("a", "b", "c")))
    a <- 1L
    b <- 2L
    expect_error(assertAllAreNonExisting(c("a", "b", "c")))
})



test_that("assertAreGeneAnnotations", {
    x <- makeGRangesFromEnsembl("Homo sapiens", format = "genes")
    expect_silent(assertAreGeneAnnotations(x))
    expect_error(
        assertAreGeneAnnotations(mtcars),
        "is_subset : "
    )
})



test_that("assertAreTranscriptAnnotations", {
    x <- makeGRangesFromEnsembl("Homo sapiens", format = "transcripts")
    expect_silent(assertAreTranscriptAnnotations(x))
    expect_error(
        assertAreTranscriptAnnotations(mtcars),
        "is_subset : "
    )
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
    genes <- pull(g2s, "geneID")
    expect_true(is.character(genes))
    x <- data.frame(
        "sample_1" = c(1L, 2L),
        "sample_2" = c(3L, 4L),
        row.names = genes,
        stringsAsFactors = FALSE
    )
    expect_silent(assertFormalGene2symbol(x, genes, g2s))
    expect_error(
        assertFormalGene2symbol(mtcars, genes, g2s),
        paste(
            "is_subset :",
            "The elements 'gene_1', 'gene_2'",
            "in genes are not in rownames\\(x\\)."
        )
    )
})



test_that("assertFormalInterestingGroups", {
    expect_silent(
        assertFormalInterestingGroups(
            x = rse_bcb,
            interestingGroups = c("tissue", "treatment")
        )
    )
    # Must exist as columns in sampleData.
    expect_error(
        assertFormalInterestingGroups(
            x = rse_bcb,
            interestingGroups = "XXX"
        ),
        "is_subset : The element 'XXX'"
    )
    # Require interesting groups to be defined as factor columns.
    expect_error(
        assertFormalInterestingGroups(
            x = rse_bcb,
            interestingGroups = c("totalReads", "exonicRate")
        ),
        "The interesting groups \"totalReads, exonicRate\" are not factor"
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
    expect_silent(assertIsAnImplicitInteger(1))  # nolint
    expect_silent(assertIsAnImplicitInteger(1L))
    expect_silent(assertIsAnImplicitInteger(1.0))
    expect_error(assertIsAnImplicitInteger(c(1L, 2L)))
    expect_error(assertIsAnImplicitInteger(1.1))
    # Check tolerance threshold.
    expect_error(assertIsImplicitInteger(1.000000000000001))
})



test_that("assertIsAnImplicitIntegerOrNULL", {
    expect_silent(assertIsAnImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1))  # nolint
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
    x <- ggplot2::scale_color_gradient(low = "red", high = "blue")
    expect_silent(assertIsColorScaleContinuousOrNULL(x))
    expect_silent(assertIsColorScaleContinuousOrNULL(NULL))
    x <- ggplot2::scale_color_manual(values = "red")
    expect_error(
        assertIsColorScaleContinuousOrNULL(x),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})



test_that("assertIsColorScaleDiscreteOrNULL", {
    x <- ggplot2::scale_color_manual(values = "red")
    expect_silent(assertIsColorScaleDiscreteOrNULL(x))
    expect_silent(assertIsColorScaleDiscreteOrNULL(NULL))
    x <- ggplot2::scale_color_gradient(low = "red", high = "blue")
    expect_error(
        assertIsColorScaleDiscreteOrNULL(x),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})



test_that("assertIsFillScaleContinuousOrNULL", {
    x <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
    expect_silent(assertIsFillScaleContinuousOrNULL(x))
    expect_silent(assertIsFillScaleContinuousOrNULL(NULL))
    x <- ggplot2::scale_fill_manual(values = "red")
    expect_error(
        assertIsFillScaleContinuousOrNULL(x),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})



test_that("assertIsFillScaleDiscreteOrNULL", {
    x <- ggplot2::scale_fill_manual(values = "red")
    expect_silent(assertIsFillScaleDiscreteOrNULL(x))
    expect_silent(assertIsFillScaleDiscreteOrNULL(NULL))
    x <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
    expect_error(
        assertIsFillScaleDiscreteOrNULL(x),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})



test_that("assertIsGene2symbol", {
    expect_silent(assertIsGene2symbol(g2s))
    expect_error(
        assertIsGene2symbol(mtcars),
        paste(
            "are_identical :",
            "colnames\\(x\\) and c\\(\"geneID\", \"geneName\"\\)",
            "are not identical."
        )
    )
    expect_error(
        assertIsGene2symbol(NULL),
        "is_data.frame : x"
    )
})



test_that("assertIsHexColorFunctionOrNULL", {
    x <- function(n) {
        colors <- c("#FFFFFF", "#000000")
        colors[seq_len(n)]
    }
    expect_silent(assertIsHexColorFunctionOrNULL(x))
    expect_error(
        assertIsHexColorFunctionOrNULL(x(2L)),
        "is2 :"
    )
    # Check viridis trailing "FF" sanitization support.
    viridis <- function(n = 2L) {
        colors <- c("#440154FF", "#FDE725FF")
        colors[n]
    }
    expect_silent(assertIsHexColorFunctionOrNULL(viridis))
})



test_that("assertIsImplicitInteger", {
    expect_silent(assertIsImplicitInteger(c(1, 2)))  # nolint
    expect_silent(assertIsImplicitInteger(c(1L, 2L)))
    expect_silent(assertIsImplicitInteger(c(1.0, 2.0)))
    expect_error(assertIsImplicitInteger(c(1.1, 2.1)))
})



test_that("assertIsImplicitIntegerOrNULL", {
    expect_silent(assertIsImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1, 2)))  # nolint
    expect_silent(assertIsImplicitIntegerOrNULL(c(1L, 2L)))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1.0, 2.0)))
    expect_error(assertIsImplicitIntegerOrNULL(c(1.1, 2.1)))
})



test_that("isImplicitInteger", {
    expect_true(isImplicitInteger(1))  # nolint
    expect_true(isImplicitInteger(1L))
    expect_true(isImplicitInteger(1.0))
    expect_false(isImplicitInteger(1.1))
})



test_that("assertIsTx2gene", {
    t2g <- tibble(
        transcriptID = c("transcript_1", "transcript_2"),
        geneID = c("gene_1", "gene_2")
    )
    expect_silent(assertIsTx2gene(t2g))
    expect_error(
        assertIsTx2gene(mtcars),
        paste(
            "are_identical :",
            "colnames\\(x\\) and c\\(\"transcriptID\", \"geneID\"\\)",
            "are not identical."
        )
    )
    expect_error(
        assertIsTx2gene(NULL),
        "is_data.frame : x"
    )
})



test_that("isAnImplicitInteger", {
    expect_identical(
        isImplicitInteger(list(1, 1L, 1.1, "XXX")),  # nolint
        c(TRUE, TRUE, FALSE, FALSE)
    )
})



test_that("isURL", {
    expect_false(isURL("XXX"))
    expect_false(isURL(1L))
})
