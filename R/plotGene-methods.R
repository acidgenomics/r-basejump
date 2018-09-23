#' Plot Gene Expression
#'
#' @name plotGene
#' @family Plot Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param countsAxisLabel `string`. Label to use for the counts axis.
#' @param medianLine `boolean`. Include median line for each group. Disabled if
#'   samples are colored by sample name.
#' @param style `string`. Plot style.
#'
#' @return
#' - `style = "facet"`: `ggplot` grouped by `sampleName`, with
#'   [ggplot2::facet_wrap()] applied to panel the samples.
#' - `style = "wide"`: `ggplot` in wide format, with genes on the x-axis.
#'
#' @examples
#' object <- rse_small
#'
#' rownames <- head(rownames(object))
#' print(rownames)
#' g2s <- gene2symbol(object)
#' geneIDs <- head(g2s[["geneID"]])
#' print(geneIDs)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' # Rownames, gene IDs, and gene names (symbols) are supported.
#' plotGene(object, genes = geneIDs, style = "facet")
#' plotGene(object, genes = geneNames, style = "wide")
NULL



.plotGeneFacet <- function(
    object,
    countsAxisLabel = "counts",
    medianLine = TRUE,
    color = NULL,
    legend = TRUE
) {
    assert_is_all_of(object, "SummarizedExperiment")
    interestingGroups <- interestingGroups(object)

    # Coerce the data to a melted tibble.
    data <- meltCounts(object)

    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("interestingGroups"),
            y = !!sym("counts"),
            color = !!sym("interestingGroups")
        )
    ) +
        .genePoint(show.legend = legend) +
        facet_wrap(facets = sym("rowname"), scales = "free_y") +
        labs(
            x = NULL,
            y = countsAxisLabel,
            color = paste(interestingGroups, collapse = ":\n")
        )

    if (
        isTRUE(medianLine) &&
        !identical(interestingGroups, "sampleName")
    ) {
        p <- p + .geneMedianLine
    }

    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }

    if (identical(interestingGroups, "sampleName")) {
        p <- p + guides(color = FALSE)
    }

    p
}



.plotGeneWide <- function(
    object,
    countsAxisLabel = "counts",
    medianLine = TRUE,
    color = NULL,
    legend = TRUE
) {
    assert_is_all_of(object, "SummarizedExperiment")
    interestingGroups <- interestingGroups(object)

    # Coerce the data to a melted tibble.
    data <- meltCounts(object)

    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("rowname"),
            y = !!sym("counts"),
            color = !!sym("interestingGroups")
        )
    ) +
        .genePoint(show.legend = legend) +
        labs(
            x = NULL,
            y = countsAxisLabel,
            color = paste(interestingGroups, collapse = ":\n")
        )

    if (
        isTRUE(medianLine) &&
        !identical(interestingGroups, "sampleName")
    ) {
        p <- p + .geneMedianLine
    }

    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }

    p
}



.plotGene.SE <-  # nolint
    function(
        object,
        genes,
        assay = 1L,
        interestingGroups = NULL,
        countsAxisLabel = "counts",
        medianLine = TRUE,
        color = getOption("basejump.discrete.color", NULL),
        legend = getOption("basejump.legend", TRUE),
        style = c("facet", "wide")
    ) {
        validObject(object)
        # Coercing to `SummarizedExperiment` for fast subsetting below.
        object <- .asSummarizedExperiment(object)
        assert_is_character(genes)
        # Limit the number of genes that can be plotted at once.
        assert_all_are_in_closed_range(length(genes), lower = 1L, upper = 20L)
        genes <- mapGenesToRownames(object, genes = genes, strict = FALSE)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        assert_is_a_bool(medianLine)
        assertIsColorScaleDiscreteOrNULL(color)
        assert_is_a_bool(legend)
        style <- match.arg(style)

        # Minimize the SE object only contain the assay of our choice.
        assay <- assays(object)[[assay]]
        assays(object) <- list(assay = assay)

        # Subset to match the genes, which have been mapped to the rownames.
        object <- object[genes, , drop = FALSE]
        # Now convert the rownames to symbols, for visualization.
        object <- convertGenesToSymbols(object)

        # Plot style.
        if (style == "facet") {
            what <- .plotGeneFacet
        } else if (style == "wide") {
            what <- .plotGeneWide
        }
        do.call(
            what = what,
            args = list(
                object = object,
                countsAxisLabel = countsAxisLabel,
                medianLine = medianLine,
                color = color,
                legend = legend
            )
        )
    }



#' @rdname plotGene
#' @export
setMethod(
    f = "plotGene",
    signature = signature("SummarizedExperiment"),
    definition = .plotGene.SE
)
