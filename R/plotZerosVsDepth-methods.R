#' Plot Percentage of Zeros vs. Library Depth
#'
#' Calculate and visualize the dropout rate.
#'
#' @name plotZerosVsDepth
#' @family Plot Functions
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `ggplot`.
#'
#' @examples
#' # SingleCellExperiment ====
#' plotZerosVsDepth(sce_small)
NULL



# zerosVsDepth =================================================================
.zerosVsDepth.matrix <-  # nolint
    function(object) {
        present <- object > 0L
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = colSums(object),
            row.names = colnames(object)
        )
    }



# Using a logical matrix is faster and more memory efficient.
# Ensure dgTMatrix gets coereced to dgCMatrix prior to logical.
.zerosVsDepth.sparseMatrix <-  # nolint
    function(object) {
        stopifnot(is(object, "sparseMatrix"))
        stopifnot(!is(object, "lgCMatrix"))
        present <- object %>%
            as("dgCMatrix") %>%
            as("lgCMatrix")
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = colSums(object),
            row.names = colnames(object)
        )
    }



.zerosVsDepth.SE <-  # nolint
    function(
        object,
        assay = 1L
    ) {
        assert_is_scalar(assay)
        counts <- assays(object)[[assay]]
        data <- zerosVsDepth(counts)
        sampleData <- sampleData(object)
        assert_are_identical(rownames(data), rownames(sampleData))
        assert_are_disjoint_sets(colnames(data), colnames(sampleData))
        cbind(data, sampleData)
    }



.zerosVsDepth.SCE <-  # nolint
    function(
        object,
        assay = 1L
    ) {
        assert_is_scalar(assay)
        counts <- assays(object)[[assay]]
        data <- zerosVsDepth(counts)
        # Add sample ID column.
        data[["sampleID"]] <- cell2sample(object)
        # Join the sample data.
        sampleData <- sampleData(object)
        sampleData[["sampleID"]] <- as.factor(rownames(sampleData))
        left_join(x = data, y = sampleData, by = "sampleID")
    }



setMethod(
    f = "zerosVsDepth",
    signature = signature("matrix"),
    definition = .zerosVsDepth.matrix
)



setMethod(
    f = "zerosVsDepth",
    signature = signature("sparseMatrix"),
    definition = .zerosVsDepth.sparseMatrix
)



setMethod(
    f = "zerosVsDepth",
    signature = signature("SummarizedExperiment"),
    definition = .zerosVsDepth.SE
)



setMethod(
    f = "zerosVsDepth",
    signature = signature("SingleCellExperiment"),
    definition = .zerosVsDepth.SCE
)



# plotZerosVsDepth =============================================================
.plotZerosVsDepth.SE <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        color = getOption("basejump.discrete.color", NULL),
        title = "zeros vs. depth"
    ) {
        validObject(object)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        assertIsColorScaleDiscreteOrNULL(color)
        assertIsAStringOrNULL(title)

        data <- zerosVsDepth(object, assay = assay)

        p <- ggplot(
            data = as(data, "tbl_df"),
            mapping = aes(
                x = !!sym("depth"),
                y = !!sym("dropout"),
                color = !!sym("interestingGroups")
            )
        ) +
            geom_point(size = 0.8, alpha = 0.8) +
            expand_limits(y = c(0L, 1L)) +
            scale_x_continuous(trans = "log10") +
            labs(
                title = title,
                x = "library size (depth)",
                y = "dropout rate",
                color = paste(interestingGroups, collapse = ":\n")
            )

        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }

        # Wrap samples by `aggregate` column, if defined.
        facets <- NULL
        if (isTRUE(.hasAggregate(data))) {
            facets <- "aggregate"
        }
        if (is.character(facets)) {
            p <- p + facet_wrap(facets = syms(facets), scales = "free")
        }

        p
    }



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature("SummarizedExperiment"),
    definition = .plotZerosVsDepth.SE
)
