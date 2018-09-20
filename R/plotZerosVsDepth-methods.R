# TODO Add SE method support.



#' Plot Percentage of Zeros vs. Library Depth
#'
#' Calculate the dropout rate.
#'
#' @name plotZerosVsDepth
#' @family Plot Functions
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `DataFrame` or `ggplot`.
#'
#' @examples
#' x <- zerosVsDepth(single_cell_counts)
#' print(x)
#'
#' plotZerosVsDepth(single_cell_counts)
NULL



.zerosVsDepth.matrix <-  # nolint
    function(object) {
        # Using a logical matrix is faster and more memory efficient.
        # Ensure dgTMatrix gets coereced to dgCMatrix prior to logical.
        present <- object > 0L
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = colSums(object),
            row.names = colnames(object)
        )
    }



.zerosVsDepth.sparseMatrix <-  # nolint
    function(object) {
        stopifnot(!is(object, "lgCMatrix"))
        # Using a logical matrix is faster and more memory efficient.
        # Ensure dgTMatrix gets coereced to dgCMatrix prior to logical.
        present <- object %>%
            as("dgCMatrix") %>%
            as("lgCMatrix")
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = colSums(object),
            row.names = colnames(object)
        )
    }



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("matrix"),
    definition = .zerosVsDepth.matrix
)



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature("sparseMatrix"),
    definition = .zerosVsDepth.sparseMatrix
)



# plotZerosVsDepth =============================================================
.plotZerosVsDepth.matrix <-  # nolint
    function(
        object,
        title = "zeros vs. depth"
    ) {
        validObject(object)
        assertIsAStringOrNULL(title)

        data <- zerosVsDepth(object) %>%
            as("tbl_df")

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("depth"),
                y = !!sym("dropout")
            )
        ) +
            geom_point(size = 0.8) +
            expand_limits(y = c(0L, 1L)) +
            scale_x_continuous(trans = "log10") +
            labs(
                title = title,
                x = "library size (depth)",
                y = "dropout rate"
            )

        p
    }



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature("matrix"),
    definition = .plotZerosVsDepth.matrix
)



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature("sparseMatrix"),
    definition = getMethod("plotZerosVsDepth", "matrix")
)
