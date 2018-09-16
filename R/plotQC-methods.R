#' Plot Quality Control
#'
#' @name plotQC
#' @family Plot Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @examples
#' plotQC(rse_small)
NULL



.plotSumsECDF <- function(object, fun) {
    assert_is_function(fun)
    data <- tibble(x = fun(object))
    ggplot(
        data = data,
        mapping = aes(x = !!sym("x"))
    ) +
        stat_ecdf(size = 1L) +
        scale_x_continuous(trans = "sqrt") +
        labs(
            x = deparse(substitute(fun)),
            y = "ECDF"
        )
}



.plotQC.SE <- function(object) {
    assay <- assay(object)
    # Counts per row (gene).
    rowSums <- .plotSumsECDF(assay, fun = rowSums) +
        labs(title = "counts per row")
    # Counts per column (sample).
    colSums <- .plotSumsECDF(assay, fun = colSums) +
        labs(title = "counts per column")
    # Dropout rate.
    zerosVsDepth <- plotZerosVsDepth(assay)
    # Return paneled plot.
    plot_grid(
        plotlist = list(
            rowSums = rowSums,
            colSums = colSums,
            zerosVsDepth = zerosVsDepth
        )
    )
}



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SummarizedExperiment"),
    definition = .plotQC.SE
)
