#' @name plotZerosVsDepth
#' @inheritParams params
#' @examples
#' data(sce)
#' plotZerosVsDepth(sce)
NULL



plotZerosVsDepth.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        color,
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
        assertIsStringOrNULL(title)

        data <- zerosVsDepth(object, assay = assay)
        assert_is_all_of(data, "DataFrame")

        p <- ggplot(
            data = as_tibble(data),
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
        if (isTRUE("aggregate" %in% colnames(data))) {
            facets <- "aggregate"
        }
        if (is.character(facets)) {
            p <- p + facet_wrap(facets = syms(facets), scales = "free")
        }

        p
    }

formals(plotZerosVsDepth.SummarizedExperiment)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature("SummarizedExperiment"),
    definition = plotZerosVsDepth.SummarizedExperiment
)
