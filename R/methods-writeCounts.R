#' Write Counts
#'
#' Supports both bulk and single-cell RNA-seq output. Bulk RNA-seq counts are
#' written to disk as comma separated values (`.csv). Single-cell RNA-seq counts
#' are written to disk in MatrixMarket format (`.mtx`), along with the sample
#' barcodes (`.colnames`), and gene identifiers (`.rownames`).
#'
#' Automatic gzip compression is offered as a user-defined option. This setting
#' is enabled by default to save disk space. Note that the
#' [readr](http://readr.tidyverse.org/) package, built into
#' [RStudio](https://www.rstudio.com/), now natively supports compressed files.
#'
#' @rdname writeCounts
#' @name writeCounts
#' @family Write Utilities
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams dots
#' @inheritParams saveData
#'
#' @param ... Count matrices, passed in as dots.
#' @param dir Output directory.
#' @param gzip Compress the counts file using gzip.
#'
#' @return No value.
#'
#' @examples
#' writeCounts(mtcars)
NULL



# Constructors ====
#' @importFrom Matrix writeMM
#' @importFrom R.utils gzip
#' @importFrom readr write_csv write_lines
#' @importFrom rlang dots_list
.writeCounts <- function(
    ...,
    dir = file.path("results", "counts"),
    gzip = TRUE,
    quiet = FALSE) {
    dots <- dots_list(...)
    hasDim <- dots %>%
        sapply(dim) %>%
        vapply(is.numeric, logical(1L))
    if (any(!hasDim)) {
        stop("Object must support dim()", call. = FALSE)
    }

    # Create the counts output directory
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    # Iterate across the dot objects and write to disk
    names <- dots(..., character = TRUE)
    if (!isTRUE(quiet)) {
        message(paste("Writing", toString(names), "to", dir))
    }
    lapply(seq_along(dots), function(a) {
        name <- names[[a]]
        counts <- dots[[a]]
        if (class(counts)[[1L]] %in% c("dgCMatrix", "dgTMatrix")) {
            # MatrixMarket file
            matrixFile <- file.path(dir, paste0(name, ".mtx"))
            writeMM(counts, matrixFile)

            # Write barcodes (colnames)
            barcodes <- colnames(counts)
            barcodesFile <- paste0(matrixFile, ".colnames")
            write_lines(barcodes, barcodesFile)

            # Write gene names (rownames)
            genes <- rownames(counts)
            genesFile <- paste0(matrixFile, ".rownames")
            write_lines(genes, genesFile)

            # gzip the matrix, if desired
            if (isTRUE(gzip)) {
                gzip(matrixFile, overwrite = TRUE)
            }
        } else {
            # Coerce to tibble use readr
            ext <- ".csv"
            if (isTRUE(gzip)) {
                ext <- paste0(ext, ".gz")
            }
            fileName <- paste0(name, ext)
            counts %>%
                as("tibble") %>%
                write_csv(path = file.path(dir, fileName))
        }
    }) %>%
        invisible()
}



# Methods ====
#' @rdname writeCounts
#' @export
setMethod(
    "writeCounts",
    signature("..." = "ANY"),
    .writeCounts)
