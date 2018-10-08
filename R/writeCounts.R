#' Write Counts
#'
#' Supports both bulk and single-cell RNA-seq count matrices. Bulk RNA-seq
#' counts are written to disk as comma separated values ("`.csv`"). Single-cell
#' RNA-seq counts are written to disk in MatrixMarket format ("`.mtx`"), along
#' with the sample barcodes ("`.colnames`"), and gene identifiers
#' ("`.rownames`").
#'
#' Automatic gzip compression is offered as a user-defined option. This setting
#' is enabled by default to save disk space. Note that the
#' [readr](http://readr.tidyverse.org/) package, built into
#' [RStudio](https://www.rstudio.com/), now natively supports compressed files.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @family Write Functions
#' @author Michael Steinbaugh, Rory Kirchner
#' @export
#'
#' @inheritParams dots
#' @inheritParams saveData
#' @param ... Count matrices, passed in as dots.
#' @param dir `string`. Output directory.
#' @param gzip `boolean`. Compress the counts file using gzip.
#'
#' @return Invisible `list`. File paths.
#'
#' @examples
#' data(rse_small, sce_small)
#' rnaseq_counts <- counts(rse_small)
#' single_cell_counts <- counts(sce_small)
#'
#' writeCounts(rnaseq_counts, single_cell_counts, dir = "example")
#' list.files("example")
#'
#' # Clean up
#' unlink("example", recursive = TRUE)
writeCounts <- function(..., dir = ".", gzip = FALSE) {
    dots <- dots_list(...)
    assert_is_list(dots)
    names <- dots(..., character = TRUE)
    dir <- initializeDirectory(dir)
    assert_is_a_bool(gzip)

    # Iterate across the dot objects and write to disk.
    message(paste0("Writing ", toString(names), " to ", dir, "..."))

    # Put the names first in the call here.
    files <- mapply(
        name <- names,
        x <- dots,
        FUN = function(name, x) {
            if (is.matrix(x)) {
                if (isTRUE(gzip)) {
                    format <- "csv.gz"
                } else {
                    format <- "csv"
                }
            } else if (is(x, "sparseMatrix")) {
                if (isTRUE(gzip)) {
                    format <- "mtx.gz"
                } else {
                    format <- "mtx"
                }
            } else {
                stop(paste(name, "is not a matrix."), call. = FALSE)
            }
            file <- file.path(dir, paste0(name, ".", format))
            do.call(what = export, args = list(x = x, file = file))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )

    invisible(files)
}
