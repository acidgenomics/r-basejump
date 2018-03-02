#' Write Counts
#'
#' Supports both bulk and single-cell RNA-seq output. Bulk RNA-seq counts are
#' written to disk as comma separated values (`.csv`). Single-cell RNA-seq
#' counts are written to disk in MatrixMarket format (`.mtx`), along with the
#' sample barcodes (`.colnames`), and gene identifiers (`.rownames`).
#'
#' Automatic gzip compression is offered as a user-defined option. This setting
#' is enabled by default to save disk space. Note that the
#' [readr](http://readr.tidyverse.org/) package, built into
#' [RStudio](https://www.rstudio.com/), now natively supports compressed files.
#'
#' @family Write Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @importFrom fs path_join
#' @importFrom Matrix writeMM
#' @importFrom R.utils gzip
#' @importFrom readr write_csv write_lines
#' @importFrom rlang dots_list
#'
#' @inheritParams dots
#' @inheritParams saveData
#'
#' @param ... Count matrices, passed in as dots.
#' @param dir Output directory.
#' @param gzip Compress the counts file using gzip.
#'
#' @return Invisible named character vector containing file paths to count
#'   matrices written to disk.
#' @export
#'
#' @examples
#' writeCounts(mtcars, gzip = TRUE)
#'
#' # Clean up
#' file_delete("mtcars.csv.gz")
writeCounts <- function(
    ...,
    dir = ".",
    gzip = TRUE) {
    dots <- dots_list(...)
    assert_is_list(dots)
    invisible(lapply(dots, assert_has_dims))
    invisible(lapply(dots, function(x) {
        assert_is_any_of(
            x,
            c(
                "data.frame",
                "dgCMatrix",
                "dgTMatrix",
                "matrix"
            )
        )
    }))
    dir <- initializeDirectory(dir)
    assert_is_a_bool(gzip)

    # Iterate across the dot objects and write to disk
    names <- dots(..., character = TRUE)

    inform(paste("Writing", toString(names), "to", dir))

    files <- lapply(seq_along(dots), function(a) {
        name <- names[[a]]
        counts <- dots[[a]]
        if (class(counts)[[1L]] %in% c("dgCMatrix", "dgTMatrix")) {
            # MatrixMarket file
            matrixFile <- path_join(c(dir, paste0(name, ".mtx")))
            writeMM(counts, matrixFile)
            # Write barcodes (colnames)
            barcodes <- colnames(counts)
            barcodesFile <- paste0(matrixFile, ".colnames")
            write_lines(barcodes, barcodesFile)
            # Write gene names (rownames)
            genes <- rownames(counts)
            genesFile <- paste0(matrixFile, ".rownames")
            write_lines(genes, genesFile)
            returnPath <- matrixFile
            # gzip the matrix, if desired
            if (isTRUE(gzip)) {
                returnPath <- gzip(matrixFile, overwrite = TRUE)
            }
        } else {
            # Coerce to tibble use readr
            ext <- ".csv"
            if (isTRUE(gzip)) {
                ext <- paste0(ext, ".gz")
            }
            fileName <- paste0(name, ext)
            filePath <- path_join(c(dir, fileName))
            # See `setAs.R` file for documentation on tibble coercion method
            write_csv(
                x = as(counts, "tibble"),
                path = filePath
            )
            returnPath <- filePath
        }
        returnPath
    })
    names(files) <- names

    invisible(files)
}
