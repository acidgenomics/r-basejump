#' Plot Heatmap
#'
#' Construct a simple heatmap.
#'
#' @section Scaling:
#'
#' Here we're scaling simply by calculating the standard score (z-score).
#'
#' - mu: mean.
#' - sigma: standard deviation.
#' - x: raw score (e.g. count matrix).
#' - z: standard score (z-score).
#'
#' ```
#' z = (x - mu) / sigma
#' ```
#'
#' See also:
#'
#' - `pheatmap:::scale_rows()`.
#' - `base::scale()` for additional scaling approaches.
#'
#' @section Hierarchical clustering:
#'
#' Row- and column-wise hierarchical clustering is performed when `clusterRows`
#' and/or `clusterCols` are set to `TRUE`. Internally, this calls `hclust()`,
#' and defaults to the Ward method.
#'
#' Automatic hierarchical clustering of rows and/or columns can error for some
#' datasets. When this occurs, you'll likely see this error:
#'
#' ```
#' Error in hclust(d, method = method) :
#' NA/NaN/Inf in foreign function call
#' ```
#'
#' In this case, either set `clusterRows` and/or `clusterCols` to `FALSE`, or
#' you can attempt to pass an `hclust` object to these arguments. This is
#' recommended as an alternate approach to be used with `pheatmap::pheatmap()`,
#' which is called internally by our plotting code. Here's how this can be
#' accomplished:
#'
#' ```
#' mat <- assay(mat)
#' dist <- dist(mat)
#' hclust <- hclust(dist, method = "ward.D2")
#' ```
#'
#' @name plotHeatmap
#' @author Michael Steinbaugh, Rory Kirchner
#' @inheritParams params
#'
#' @param borderColor `string` or `NULL`. Border color. Disabled by default for
#'   improved aesthetics.
#' @param clusteringMethod `string`. Clustering method. Accepts the same values
#'   as `stats::hclust()`.
#' @param clusterRows,clusterCols `boolean`. Arrange with hierarchical
#'   clustering.
#' @param color `function`, `character`, or `NULL`. Hexadecimal color function
#'   or values to use for plot. We generally recommend these hexadecimal
#'   functions from the viridis package:
#'   - `viridis::viridis()` (*default*).
#'   - `viridis::inferno()`.
#'   - `viridis::magma()`.
#'   - `viridis::plasma()`.
#'   Alternatively, colors can be defined manually using hexadecimal values
#'   (e.g. `c("#FF0000", "#0000FF")`), but this is not generally recommended.
#'   Refer to the RColorBrewer package for hexadecimal color palettes that may
#'   be suitable. If set `NULL`, will use the default pheatmap colors.
#' @param legend `boolean`. Show the color legend.
#' @param legendColor `function` or `NULL`. Hexadecimal color function to use
#'   for legend labels. Note that hexadecimal values are not supported. If set
#'   `NULL`, will use the default pheatmap colors.
#' @param method `string`. Correlation coefficient (or covariance) method to be
#'   computed. Defaults to "`pearson`" but "`spearman`" can also be used.
#'   Consult the `stats::cor()` documentation for more information.
#' @param n `scalar integer`. The number of quantile breaks to create.
#' @param scale `string`. Whether the values should be centered and scaled in
#'   either the row or column direction ("`row`", "`column`"), or remain
#'   unscaled ("`none`").
#' @param showRownames,showColnames `boolean`. Show row or column names.
#' @param treeheightRow,treeheightCol `scalar integer`. Size of the row and
#'   column dendrograms. Use `0` to disable.
#' @param title `string` or `NULL`. Plot title.
#' @param ... Passthrough arguments to `pheatmap::pheatmap()`. The names of the
#'   arguments should be formatted in camel case, not snake case.
#'
#' @seealso
#' - `pheatmap::pheatmap()`.
#' - `RColorBrewer::brewer.pal()`.
#'
#' @return `pheatmap`.
#'
#' @seealso
#' - `pheatmap::pheatmap()`.
#' - `stats::cor()`.
#' - `stats::hclust()`.
#'
#' @examples
#' data(rse, sce)
#'
#' ## SummarizedExperiment ====
#' plotHeatmap(rse)
#' plotCorrelationHeatmap(rse)
#' plotQuantileHeatmap(rse)
#'
#' ## Disable column clustering.
#' plotHeatmap(rse, clusterCols = FALSE)
#'
#' ## Using pheatmap default colors.
#' plotHeatmap(rse, color = NULL, legendColor = NULL)
#'
#' ## Using hexadecimal color input.
#' library(RColorBrewer)
#' purple_orange <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
#' plotHeatmap(rse, color = purple_orange)
#'
#' ## SingleCellExperiment ====
#' plotHeatmap(sce)
#' plotCorrelationHeatmap(sce)
#' plotQuantileHeatmap(sce)
NULL



.scaleRows <- function(object) {
    mean <- apply(object, MARGIN = 1L, FUN = mean, na.rm = TRUE)
    sd <- apply(object, MARGIN = 1L, FUN = sd, na.rm = TRUE)
    (object - mean) / sd
}



# Modified version of `pheatmap:::scale_mat()`.
.scaleMatrix <- function(object, scale = c("none", "row", "column")) {
    assert(is.matrix(object))
    scale <- match.arg(scale)
    if (scale != "none") {
        message(paste0("Scaling matrix per ", scale, " (z-score)."))
    }
    # Assert checks to look for sufficient variance when the user is attempting
    # to apply scaling (z-score).
    varThreshold <- 0L
    if (scale == "row") {
        pass <- rowVars(object) > varThreshold
        if (!all(pass)) {
            fail <- !pass
            stop(paste0(
                "Rows cannot be scaled.\n",
                sum(fail, na.rm = TRUE),
                " rows(s) don't have enough variance.\n",
                printString(rownames(object)[which(fail)], max = 10L)
            ))
        }
    } else if (scale == "column") {
        pass <- colVars(object) > varThreshold
        if (!all(pass)) {
            fail <- !pass
            stop(paste(
                "Columns cannot be scaled.\n",
                sum(fail, na.rm = TRUE),
                " column(s) don't have enough variance.\n",
                printString(colnames(object)[which(fail)], max = 10L)
            ))
        }
    }
    switch(
        EXPR = scale,
        none = object,
        row = .scaleRows(object),
        column = t(.scaleRows(t(object)))
    )
}



plotHeatmap.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        scale = c("none", "row", "column"),
        clusteringMethod = "ward.D2",
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 50L,  # set to `0L` to disable.
        treeheightCol = 50L,  # set to `0L` to disable.
        color = viridis::viridis,
        legendColor = viridis::viridis,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert(
            nrow(object) > 1L,
            ncol(object) > 1L,
            isScalar(assay),
            isFlag(clusterRows),
            isFlag(clusterCols),
            isFlag(showRownames),
            isFlag(showColnames),
            isInt(treeheightRow),
            isInt(treeheightCol),
            isString(borderColor) || is.null(borderColor),
            isString(title) || is.null(title)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        scale <- match.arg(scale)
        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (!isString(title)) {
            title <- NA
        }

        # Warn and early return if any samples are duplicated.
        # We've included this step here to work with the minimal bcbio RNA-seq
        # test data set, which contains duplicate samples.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }

        # Convert the SE object to use symbols in the rownames, for pheatmap.
        suppressMessages(
            object <- convertGenesToSymbols(object)
        )

        # Ensure we're always using a dense matrix.
        mat <- as.matrix(assays(object)[[assay]])

        # Ensure the user isn't passing in a matrix with any rows or columns
        # containing all zeros when we're attempting to z-scale.
        if (scale != "none") {
            assert(hasNonZeroRowsAndCols(mat))
        }

        # Pre-process the matrix by applying row/column scaling, if desired.
        # Run this step before hierarchical clustering (i.e. calculating the
        # distance matrix).
        mat <- .scaleMatrix(mat, scale = scale)

        # Now we're ready to perform hierarchical clustering. Generate `hclust`
        # objects for rows and columns that we'll pass to pheatmap. Note that
        # pheatmap supports `clusterRows = TRUE` and `clusterCols = TRUE`, but
        # these have been found to error for some datasets. Therefore, we're
        # performing hclust calculations on own here.
        if (isTRUE(clusterRows) || isTRUE(clusterCols)) {
            message(paste0(
                "Performing hierarchical clustering.\n",
                "Using stats::hclust(method = ", deparse(clusteringMethod), ")."
            ))
            if (isTRUE(clusterRows)) {
                message("Arranging rows using hclust.")
                clusterRows <- tryCatch(
                    expr = hclust(
                        d = dist(mat),
                        method = clusteringMethod
                    ),
                    error = function(e) {
                        warning(
                            "hclust() row calculation failed. Skipping.",
                            call. = FALSE
                        )
                        FALSE
                    }
                )
            }
            if (isTRUE(clusterCols)) {
                message("Arranging columns using hclust.")
                clusterCols <- tryCatch(
                    expr = hclust(
                        # Note the use of `t()` here.
                        d = dist(t(mat)),
                        method = clusteringMethod
                    ),
                    error = function(e) {
                        warning(
                            "hclust() column calculation failed. Skipping.",
                            call. = FALSE
                        )
                        FALSE
                    }
                )
            }
        }

        # Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(object = object, legendColor = legendColor)
        assert(
            is.list(x),
            identical(
                x = names(x),
                y = c("annotationCol", "annotationColors")
            )
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]
        color <- .pheatmapColorPalette(color)

        # Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (length(sampleNames) > 0L) {
            colnames(mat) <- sampleNames
            if (
                length(annotationCol) > 0L &&
                !is.na(annotationCol)
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }

        # Return pretty heatmap with modified defaults.
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            clusterCols = clusterCols,
            clusterRows = clusterRows,
            color = color,
            main = title,
            # We're already applied scaling manually (see above).
            scale = "none",
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        do.call(what = pheatmap, args = args)
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotHeatmap.SummarizedExperiment
)



plotHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(object, fun = "mean")
        do.call(
            what = plotHeatmap,
            args = matchArgsToDoCall(
                args = list(object = agg)
            )
        )
    }

formals(plotHeatmap.SingleCellExperiment) <-
    formals(plotHeatmap.SummarizedExperiment)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotHeatmap.SingleCellExperiment
)



plotCorrelationHeatmap.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        method = c("pearson", "spearman"),
        clusteringMethod = "ward.D2",
        showRownames = TRUE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legendColor = viridis::viridis,
        borderColor = NULL,
        title = TRUE,
        ...
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            nrow(object) > 1L,
            ncol(object) > 1L,
            isString(clusteringMethod),
            isFlag(showRownames),
            isFlag(showColnames),
            isInt(treeheightRow),
            isInt(treeheightCol),
            isString(borderColor) || is.null(borderColor)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        method <- match.arg(method)
        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (isTRUE(title)) {
            title <- paste(method, "correlation")
        } else if (!isString(title)) {
            title <- NA
        }
        # Warn and early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }

        # Correlation matrix.
        mat <- as.matrix(assays(object)[[assay]])
        mat <- cor(mat, method = method)

        # Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(object = object, legendColor = legendColor)
        assert(
            is.list(x),
            identical(
                x = names(x),
                y = c("annotationCol", "annotationColors")
            )
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]
        color <- .pheatmapColorPalette(color)

        # Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (length(sampleNames) > 0L) {
            rownames(mat) <- sampleNames
            colnames(mat) <- sampleNames
            if (
                length(annotationCol) > 0L &&
                !is.na(annotationCol)
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }

        # Return pretty heatmap with modified defaults.
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            clusteringMethod = clusteringMethod,
            clusteringDistanceRows = "correlation",
            clusteringDistanceCols = "correlation",
            color = color,
            main = title,
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        assert(areDisjointSets(names(args), "scale"))
        do.call(what = pheatmap, args = args)
    }



#' @describeIn plotHeatmap Construct a correlation heatmap comparing the columns
#'   of the matrix.
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotCorrelationHeatmap.SummarizedExperiment
)



plotCorrelationHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(object, fun = "mean")
        do.call(
            what = plotCorrelationHeatmap,
            args = matchArgsToDoCall(
                args = list(object = agg)
            )
        )
    }

formals(plotCorrelationHeatmap.SingleCellExperiment) <-
    formals(plotCorrelationHeatmap.SummarizedExperiment)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotCorrelationHeatmap.SingleCellExperiment
)



.quantileBreaks <- function(object, n = 10L) {
    assert(
        is.matrix(object),
        isInt(n),
        isPositive(n)
    )
    breaks <- quantile(object, probs = seq(0L, 1L, length.out = n))
    breaks[!duplicated(breaks)]
}



plotQuantileHeatmap.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        n = 10L,
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color = viridis::viridis,
        legendColor = viridis::viridis,
        legend = FALSE,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert(
            nrow(object) > 1L,
            ncol(object) > 1L,
            isScalar(assay),
            isInt(n),
            isFlag(clusterCols),
            isFlag(clusterRows),
            isFlag(legend),
            isString(borderColor) || is.null(borderColor),
            isString(title) || is.null(title)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        n <- as.integer(n)

        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (!isString(title)) {
            title <- NA
        }

        # Warn and early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }

        # Convert the SE object to use symbols in the rownames, for pheatmap.
        suppressWarnings(
            object <- convertGenesToSymbols(object)
        )

        # Ensure we're using a dense matrix.
        mat <- as.matrix(assays(object)[[assay]])

        # Calculate the quantile breaks.
        breaks <- .quantileBreaks(mat, n = n)

        # Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(object = object, legendColor = legendColor)
        assert(
            is.list(x),
            identical(names(x), c("annotationCol", "annotationColors"))
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]

        # Note the number of breaks here.
        color <- .pheatmapColorPalette(color, n = length(breaks) - 1L)

        # Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (length(sampleNames) > 0L) {
            colnames(mat) <- sampleNames
            if (
                length(annotationCol) > 0L &&
                !is.na(annotationCol)
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }

        # Return pretty heatmap with modified defaults.
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            breaks = breaks,
            clusterCols = clusterCols,
            clusterRows = clusterRows,
            color = color,
            legend = legend,
            legendBreaks = breaks,
            legendLabels = round(breaks, digits = 2L),
            main = title,
            scale = "none",
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        do.call(what = pheatmap, args = args)
    }



#' @describeIn plotHeatmap Scale the heatmap by applying quantile breaks.
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotQuantileHeatmap.SummarizedExperiment
)



plotQuantileHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(object, fun = "mean")
        do.call(
            what = plotQuantileHeatmap,
            args = matchArgsToDoCall(
                args = list(object = agg)
            )
        )
    }

formals(plotQuantileHeatmap.SingleCellExperiment) <-
    formals(plotQuantileHeatmap.SummarizedExperiment)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotQuantileHeatmap.SingleCellExperiment
)



.emptyPheatmapAnnotations <- list(
    annotationCol = NA,
    annotationColors = NA
)



# Automatically handle the annotation data and colors.
# Factors with a single level are automatically dropped.
.pheatmapAnnotations <- function(
    object,
    blacklist = "sampleName",
    legendColor
) {
    validObject(object)
    assert(
        isCharacter(blacklist),
        isHexColorFunction(legendColor) || is.null(legendColor)
    )

    # Annotation columns -------------------------------------------------------
    data <- colData(object)
    interestingGroups <- interestingGroups(object)

    # pheatmap requires `NA` if empty.
    if (
        !hasDims(data) ||
        length(interestingGroups) == 0L ||
        identical(interestingGroups, "sampleName")
    ) {
        return(.emptyPheatmapAnnotations)
    }

    assert(hasRownames(data))
    data <- data[, interestingGroups, drop = FALSE]

    # Prepare the blacklist, always excluding sample names from labeling in
    # the pheatmap annotation columns.
    blacklist <- unique(c("sampleName", blacklist))

    data <- data %>%
        as_tibble(rownames = "rowname") %>%
        # Remove blacklisted columns (e.g. `sampleName`).
        .[, setdiff(colnames(.), blacklist), drop = FALSE] %>%
        # Ensure all strings are factors.
        mutate_if(is.character, as.factor) %>%
        # Ensure unwanted columns like `sizeFactor` are dropped.
        select_if(is.factor) %>%
        as.data.frame() %>%
        column_to_rownames("rowname")

    # Drop any remaining factor columns that contain a single level.
    hasLevels <- vapply(
        X = data,
        FUN = function(x) {
            length(levels(x)) > 1L
        },
        FUN.VALUE = logical(1L)
    )

    # Return empty if there are no useful factor columns.
    if (length(hasLevels) == 0L) {
        return(.emptyPheatmapAnnotations)  # nocov
    }

    data <- data[, hasLevels, drop = FALSE]

    # Colors -------------------------------------------------------------------
    if (
        is.data.frame(data) &&
        is.function(legendColor)
    ) {
        colors <- lapply(
            X = data,
            FUN = function(x) {
                assert(is.factor(x))
                levels <- levels(x)
                colors <- legendColor(length(levels))
                names(colors) <- levels
                colors
            })
        names(colors) <- colnames(data)
    } else {
        colors <- NA
    }

    # Return -------------------------------------------------------------------
    list(
        annotationCol = data,
        annotationColors = colors
    )
}



# Sanitize formals into snake case and abort on duplicates.
# Duplicates may arise if user is mixing and matching camel/snake case.
.pheatmapArgs <- function(args) {
    assert(is.list(args), hasNames(args))
    # Abort on snake case formatted formal args.
    invalidNames <- grep("[._]", names(args), value = TRUE)
    if (length(invalidNames) > 0L) {
        stop(paste(
            "Specify arguments in camel case:",
            toString(invalidNames)
        ))
    }
    names(args) <- snake(names(args))
    assert(
        isSubset(names(args), formalArgs(pheatmap)),
        hasNoDuplicates(names(args))
    )

    args
}



# If `color = NULL`, use the pheatmap default palette
.pheatmapColorPalette <- function(color = NULL, n = 256L) {
    if (is.character(color)) {
        # Hexadecimal color palette (e.g. RColorBrewer, viridis return).
        assert(containsHexColors(color))
        color
    } else if (is.function(color)) {
        # Hexadecimal color function (e.g. viridis functions).
        assert(isHexColorFunction(color))
        color(n)
    } else {
        # pheatmap default palette.
        # Note that `n` argument won't get evaluated here.
        eval(formals(pheatmap)[["color"]])
    }
}
