#' Write Counts
#'
#' Supports both bulk and single-cell RNA-seq count matrices. Bulk RNA-seq
#' counts are written to disk as comma separated values ("`.CSV`"). Single-cell
#' RNA-seq counts are written to disk in MatrixMarket format ("`.MTX`"), along
#' with the sample barcodes ("`.COLNAMES`"), and gene identifiers
#' ("`.ROWNAMES`").
#'
#' Automatic gzip compression is offered as a user-defined option. This setting
#' is enabled by default to save disk space. Note that the [readr][] package,
#' built into [RStudio][], now natively supports compressed files.
#'
#' [readr]: http://readr.tidyverse.org/
#' [RStudio]: https://www.rstudio.com/
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @author Michael Steinbaugh, Rory Kirchner
#' @inheritParams dots
#' @inheritParams saveData
#' @export
#'
#' @param ... Symbols.
#'   Unquoted object names containing count matrices.
#' @param dir `character(1)`.
#'   Output directory.
#' @param compress `logical(1)`.
#'   Compress the files using gzip.
#'
#' @return Invisible `list`.
#' File paths.
#'
#' @examples
#' library(SummarizedExperiment)
#' library(SingleCellExperiment)
#'
#' data(rse, sce)
#'
#' rnaseq_counts <- SummarizedExperiment::assay(rse)
#' single_cell_counts <- SummarizedExperiment::assay(sce)
#'
#' writeCounts(rnaseq_counts, single_cell_counts, dir = "example")
#' list.files("example")
#'
#' ## Clean up.
#' unlink("example", recursive = TRUE)
writeCounts <- function(..., dir = ".", compress = FALSE) {
    # Catch legacy arguments.
    call <- match.call()
    if ("gzip" %in% names(call)) {
        stop("Use `compress` instead of `gzip`.")
    }

    dots <- dots_list(...)
    assert(is.list(dots))
    names <- dots(..., character = TRUE)
    dir <- initDir(dir)
    assert(isFlag(compress))

    # Iterate across the dot objects and write to disk.
    message(paste0("Writing ", toString(names), " to ", dir, "."))

    # Put the names first in the call here.
    files <- mapply(
        name <- names,
        x <- dots,
        FUN = function(name, x) {
            if (is.matrix(x)) {
                if (isTRUE(compress)) {
                    format <- "csv.gz"
                } else {
                    format <- "csv"
                }
            } else if (is(x, "sparseMatrix")) {
                if (isTRUE(compress)) {
                    format <- "mtx.gz"
                } else {
                    format <- "mtx"
                }
            } else {
                stop(paste(name, "is not a matrix."))
            }
            file <- file.path(dir, paste0(name, ".", format))
            do.call(what = export, args = list(x = x, file = file))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )

    invisible(files)
}
