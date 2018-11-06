#' @name plotCountsPerGene
#' @inherit basejump.generics::plotCountsPerGene
#' @inheritParams basejump.globals::params
#'
#' @param geom `string`. Type of ggplot2 geometric object to use.
#'
#' @examples
#' data(rse, sce, package = "basejump.data")
#'
#' ## SummarizedExperiment ====
#' plotCountsPerGene(rse, geom = "boxplot")
#' plotCountsPerGene(rse, geom = "density")
#'
#' ## SingleCellExperiment ====
#' plotCountsPerGene(sce)
NULL



#' @importFrom basejump.generics plotCountsPerGene
#' @aliases NULL
#' @export
basejump.generics::plotCountsPerGene



plotCountsPerGene.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        geom = c("boxplot", "density", "violin"),
        trans = c("identity", "log2", "log10"),
        color = getOption("basejump.discrete.color", NULL),
        fill = getOption("basejump.discrete.fill", NULL),
        flip = getOption("basejump.flip", TRUE),
        countsAxisLabel = "counts",
        title = "counts per gene"
    ) {
        validObject(object)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        assertIsFillScaleDiscreteOrNULL(fill)
        assert_is_a_bool(flip)
        assertIsAStringOrNULL(countsAxisLabel)
        assertIsAStringOrNULL(title)

        data <- meltCounts(
            object = object,
            assay = assay,
            nonzeroGenes = TRUE,
            trans = trans
        )

        # Subtitle
        if (is_a_string(title)) {
            count <- length(unique(data[["rowname"]]))
            subtitle <- paste(count, "non-zero genes")
        } else {
            subtitle <- NULL
        }

        # Construct the ggplot.
        p <- ggplot(data = data)

        if (geom == "density") {
            p <- p +
                geom_density(
                    mapping = aes(
                        x = !!sym("counts"),
                        group = !!sym("interestingGroups"),
                        color = !!sym("interestingGroups")
                    ),
                    fill = NA,
                    size = 1L
                ) +
                labs(x = countsAxisLabel)
        } else if (geom == "boxplot") {
            p <- p +
                geom_boxplot(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("counts"),
                        fill = !!sym("interestingGroups")
                    ),
                    color = "black"
                ) +
                labs(x = NULL, y = countsAxisLabel)
        } else if (geom == "violin") {
            p <- p +
                geom_violin(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("counts"),
                        fill = !!sym("interestingGroups")
                    ),
                    color = "black",
                    scale = "width"
                ) +
                labs(x = NULL, y = countsAxisLabel)
        }

        # Add the axis and legend labels.
        p <- p +
            labs(
                title = title,
                subtitle = subtitle,
                color = paste(interestingGroups, collapse = ":\n"),
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }

        # Flip the axis for plots with counts on y-axis, if desired.
        if (isTRUE(flip) && !geom %in% "density") {
            p <- p + coord_flip()
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(color = FALSE, fill = FALSE)
        }

        p
    }



plotCountsPerGene.SingleCellExperiment <-  # nolint
    function(object) {
        do.call(
            what = plotCountsPerGene,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }
formals(plotCountsPerGene.SingleCellExperiment) <-
    formals(plotCountsPerGene.SummarizedExperiment)



#' @rdname plotCountsPerGene
#' @export
setMethod(
    f = "plotCountsPerGene",
    signature = signature("SummarizedExperiment"),
    definition = plotCountsPerGene.SummarizedExperiment
)



#' @rdname plotCountsPerGene
#' @export
setMethod(
    f = "plotCountsPerGene",
    signature = signature("SingleCellExperiment"),
    definition = plotCountsPerGene.SingleCellExperiment
)
