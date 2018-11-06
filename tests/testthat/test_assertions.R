context("Assertions")

data(rse, tx_se, package = "basejump.data", envir = environment())

# nolint start
DataFrame <- S4Vectors::DataFrame
Gene2Symbol <- basejump.classes::Gene2Symbol
as_tibble <- tibble::as_tibble
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
tibble <- tibble::tibble
# nolint end

g2s <- Gene2Symbol(
    object = DataFrame(
        geneID = paste0("gene", seq_len(2L)),
        geneName = paste0("symbol", seq_len(2L))
    )
)



# assertAllAreNonExisting ======================================================
test_that("assertAllAreNonExisting", {
    expect_silent(assertAllAreNonExisting(c("a", "b", "c")))
    # Error on existing values in environment.
    a <- 1L
    b <- 2L
    expect_error(assertAllAreNonExisting(c("a", "b", "c")))
})



# assertAreGeneAnnotations =====================================================
test_that("assertAreGeneAnnotations", {
    object <- rowRanges(rse)
    expect_silent(assertAreGeneAnnotations(object))
    expect_error(
        object = assertAreGeneAnnotations(mtcars),
        regexp = "is_subset : "
    )
})



# assertAreTranscriptAnnotations ===============================================
test_that("assertAreTranscriptAnnotations", {
    object <- rowData(tx_se)
    expect_silent(assertAreTranscriptAnnotations(object))
    expect_error(
        object = assertAreTranscriptAnnotations(mtcars),
        regexp = "is_subset : "
    )
})



# assertFormalCompress =========================================================
test_that("assertFormalCompress", {
    expect_error(
        object = assertFormalCompress("XXX"),
        regexp = paste(
            "is_subset :",
            "The element 'XXX' in object is not in",
            "c\\(\"bzip2\", \"gzip\", \"xz\"\\)."
        )
    )
    expect_silent(assertFormalCompress("xz"))
})



# assertFormalGene2Symbol ======================================================
test_that("assertFormalGene2Symbol", {
    genes <- g2s[["geneID"]]
    expect_true(is.character(genes))
    object <- DataFrame(
        "sample1" = c(1L, 2L),
        "sample2" = c(3L, 4L),
        row.names = genes
    )
    expect_null(
        assertFormalGene2Symbol(
            object = object,
            genes = genes,
            gene2symbol = g2s
        )
    )
    expect_error(
        object = assertFormalGene2Symbol(
            object = mtcars,
            genes = genes,
            gene2symbol = g2s
        ),
        regexp = "are_identical :"
    )
})



# assertFormalInterestingGroups ================================================
test_that("assertFormalInterestingGroups", {
    expect_silent(
        assertFormalInterestingGroups(
            object = rse,
            interestingGroups = c("genotype", "treatment")
        )
    )

    # Must exist as columns in sampleData.
    expect_error(
        object = assertFormalInterestingGroups(
            object = rse,
            interestingGroups = "XXX"
        ),
        regexp = "is_subset : The element 'XXX'"
    )
})



# assertHasRownames ============================================================
test_that("assertHasRownames", {
    df <- data.frame(
        x = seq_len(2L),
        y = seq_len(2L),
        row.names = letters[seq_len(2L)]
    )
    expect_silent(
        assertHasRownames(df)
    )
    expect_silent(
        assertHasRownames(as(df, "DataFrame"))
    )
    expect_silent(
        assertHasRownames(as_tibble(df, rownames = "rowname"))
    )

    expect_error(
        assertHasRownames(DataFrame())
    )
    expect_error(
        assertHasRownames(data.frame())
    )
    expect_error(
        assertHasRownames(tibble())
    )
})



# assertIsHeaderLevel ==========================================================
test_that("assertIsHeaderLevel", {
    expect_silent(assertIsHeaderLevel(1L))
    expect_error(
        object = assertIsHeaderLevel(8L),
        regexp = paste(
            "is_subset :",
            "The element '8' in as.integer\\(object\\)"
        )
    )
})



# assertIsAnImplicitInteger ====================================================
test_that("assertIsAnImplicitInteger", {
    expect_silent(assertIsAnImplicitInteger(1))  # nolint
    expect_silent(assertIsAnImplicitInteger(1L))
    expect_silent(assertIsAnImplicitInteger(1.0))
    expect_error(assertIsAnImplicitInteger(c(1L, 2L)))
    expect_error(assertIsAnImplicitInteger(1.1))
    # Check tolerance threshold.
    expect_error(assertIsImplicitInteger(1.000000000000001))
})



test_that("isAnImplicitInteger", {
    expect_identical(
        isImplicitInteger(list(1, 1L, 1.1, "XXX")),  # nolint
        c(TRUE, TRUE, FALSE, FALSE)
    )
})



# assertIsAnImplicitIntegerOrNULL ==============================================
test_that("assertIsAnImplicitIntegerOrNULL", {
    expect_silent(assertIsAnImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1))  # nolint
    expect_silent(assertIsAnImplicitIntegerOrNULL(1L))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1.0))
    expect_error(assertIsAnImplicitIntegerOrNULL(1.1))
})



# assertIsAnIntegerOrNULL ======================================================
test_that("assertIsAnIntegerOrNULL", {
    expect_silent(assertIsAnIntegerOrNULL(1L))
    expect_silent(assertIsAnIntegerOrNULL(NULL))
    expect_error(
        object = assertIsAnIntegerOrNULL(c(1L, 2L)),
        regexp = "is_an_integer : object has length 2, not 1."
    )
})



# assertIsANumberOrNULL ========================================================
test_that("assertIsANumberOrNULL", {
    expect_silent(assertIsANumberOrNULL(1.1))
    expect_silent(assertIsANumberOrNULL(NULL))
    expect_error(
        object = assertIsANumberOrNULL(c(1.1, 1.2)),
        regexp = "is_a_number : object has length 2, not 1."
    )
})



# assertIsAStringOrNULL ========================================================
test_that("assertIsAStringOrNULL", {
    expect_silent(assertIsAStringOrNULL("hello world"))
    expect_silent(assertIsAStringOrNULL(NULL))
    expect_error(
        object = assertIsAStringOrNULL(c("hello", "world")),
        regexp = "is_a_string : object has length 2, not 1."
    )
})



# assertIsColorScaleContinuousOrNULL ===========================================
test_that("assertIsColorScaleContinuousOrNULL", {
    object <- ggplot2::scale_color_gradient(low = "red", high = "blue")
    expect_silent(assertIsColorScaleContinuousOrNULL(object))
    expect_silent(assertIsColorScaleContinuousOrNULL(NULL))
    object <- ggplot2::scale_color_manual(values = "red")
    expect_error(
        object = assertIsColorScaleContinuousOrNULL(object),
        regexp = paste(
            "is2 :",
            "object is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})



# assertIsColorScaleDiscreteOrNULL =============================================
test_that("assertIsColorScaleDiscreteOrNULL", {
    object <- ggplot2::scale_color_manual(values = "red")
    expect_silent(assertIsColorScaleDiscreteOrNULL(object))
    expect_silent(assertIsColorScaleDiscreteOrNULL(NULL))
    object <- ggplot2::scale_color_gradient(low = "red", high = "blue")
    expect_error(
        object = assertIsColorScaleDiscreteOrNULL(object),
        regexp = paste(
            "is2 :",
            "object is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})



# assertIsFillScaleContinuousOrNULL ============================================
test_that("assertIsFillScaleContinuousOrNULL", {
    object <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
    expect_silent(assertIsFillScaleContinuousOrNULL(object))
    expect_silent(assertIsFillScaleContinuousOrNULL(NULL))
    object <- ggplot2::scale_fill_manual(values = "red")
    expect_error(
        object = assertIsFillScaleContinuousOrNULL(object),
        regexp = paste(
            "is2 :",
            "object is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})



# assertIsFillScaleDiscreteOrNULL ==============================================
test_that("assertIsFillScaleDiscreteOrNULL", {
    object <- ggplot2::scale_fill_manual(values = "red")
    expect_silent(assertIsFillScaleDiscreteOrNULL(object))
    expect_silent(assertIsFillScaleDiscreteOrNULL(NULL))
    object <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
    expect_error(
        object = assertIsFillScaleDiscreteOrNULL(object),
        regexp = paste(
            "is2 :",
            "object is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})



# assertIsHexColorFunctionOrNULL ===============================================
test_that("assertIsHexColorFunctionOrNULL", {
    object <- function(n) {
        colors <- c("#FFFFFF", "#000000")
        colors[seq_len(n)]
    }
    expect_silent(assertIsHexColorFunctionOrNULL(object))
    expect_error(
        object = assertIsHexColorFunctionOrNULL(object(2L)),
        regexp = "is2 :"
    )
    # Check viridis trailing "FF" sanitization support.
    viridis <- function(n = 2L) {
        colors <- c("#440154FF", "#FDE725FF")
        colors[n]
    }
    expect_silent(assertIsHexColorFunctionOrNULL(viridis))
})



# assertIsImplicitInteger ======================================================
test_that("assertIsImplicitInteger", {
    expect_silent(assertIsImplicitInteger(c(1, 2)))  # nolint
    expect_silent(assertIsImplicitInteger(c(1L, 2L)))
    expect_silent(assertIsImplicitInteger(c(1.0, 2.0)))
    expect_error(assertIsImplicitInteger(c(1.1, 2.1)))
})



# assertIsImplicitIntegerOrNULL ================================================
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



# assertIsURL ==================================================================
# TODO Need to improve this.
test_that("isURL", {
    expect_false(isURL("XXX"))
    expect_false(isURL(1L))
})
