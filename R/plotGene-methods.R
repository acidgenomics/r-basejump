# TODO Export `plotGeneExpression` instead or define as an alias?



#' Plot Gene Expression
#'
#' @name plotGene
#' @inheritParams params
#'
#' @param countsAxisLabel `character(1)`.
#'   Label to use for the counts axis.
#' @param medianLine `logical(1)`.
#'   Include median line for each group. Disabled if samples are colored by
#'   sample name.
#' @param style `character(1)`.
#'   Plot style.
#'
#' @return
#' - `style = "facet"`: `ggplot` grouped by `sampleName`, with
#'   `ggplot2::facet_wrap` applied to panel the samples.
#' - `style = "wide"`: `ggplot` in wide format, with genes on the x-axis.
#'
#' @examples
#' data(rse)
#' object <- rse
#'
#' rownames <- head(rownames(object))
#' print(rownames)
#' g2s <- Gene2Symbol(object)
#' geneIDs <- head(g2s[["geneID"]])
#' print(geneIDs)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' ## Rownames, gene IDs, and gene names (symbols) are supported.
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
    assert(is(object, "SummarizedExperiment"))
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
    assert(is(object, "SummarizedExperiment"))
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



plotGene.SummarizedExperiment <-  # nolint
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
        assert(
            isCharacter(genes),
            # Limit the number of genes that can be plotted at once.
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isFlag(medianLine),
            isGGScale(color, scale = "discrete", aes = "colour") ||
                is.null(color),
            isFlag(legend)
        )

        # Coercing to `SummarizedExperiment` for fast subsetting below.
        object <- as.SummarizedExperiment(object)
        genes <- mapGenesToRownames(object, genes = genes, strict = FALSE)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        style <- match.arg(style)

        # Minimize the SE object only contain the assay of our choice.
        assay <- assays(object)[[assay]]
        assays(object) <- list(assay = assay)

        # Subset to match the genes, which have been mapped to the rownames.
        object <- object[genes, , drop = FALSE]
        # Now convert the rownames to symbols, for visualization.
        suppressMessages(
            object <- convertGenesToSymbols(object)
        )

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
    definition = plotGene.SummarizedExperiment
)



#' @rdname plotGene
#' @usage NULL
#' @export
setMethod(
    f = "plotGene",
    signature = signature("SingleCellExperiment"),
    definition = function(object, ...) {
        .Deprecated("pointillism::plotGene")
    }
)
