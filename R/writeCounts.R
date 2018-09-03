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
#' @family Write Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams dots
#' @inheritParams saveData
#' @param ... Count matrices, passed in as dots.
#' @param dir `string`. Output directory.
#' @param gzip `boolean`. Compress the counts file using gzip.
#'
#' @return Invisible named `character` containing file paths.
#' @export
#'
#' @examples
#' writeCounts(rnaseq_counts, single_cell_counts, dir = "example")
#' list.files("example")
#'
#' # Clean up
#' unlink("example", recursive = TRUE)
writeCounts <- function(
    ...,
    dir = ".",
    gzip = TRUE
) {
    dots <- dots_list(...)
    assert_is_list(dots)
    names <- dots(..., character = TRUE)
    dir <- initializeDirectory(dir)
    assert_is_a_bool(gzip)

    # Iterate across the dot objects and write to disk.
    message(paste("Writing", toString(names), "to", dir))

    files <- mapply(
        name <- names,
        counts <- dots,
        FUN = function(name, counts) {
            if (is.matrix(counts)) {
                # Coercing matrix to tibble here, to keep rownames intact.
                ext <- ".csv"
                if (isTRUE(gzip)) {
                    ext <- paste0(ext, ".gz")
                }
                fileName <- paste0(name, ext)
                filePath <- file.path(dir, fileName)
                write_csv(
                    x = as(counts, "tbl_df"),
                    path = filePath
                )
                returnPath <- filePath
            } else if (grepl("^dg.+Matrix$", class(counts)[[1L]])) {
                # MatrixMarket file
                matrixFile <- file.path(dir, paste0(name, ".mtx"))
                writeMM(counts, matrixFile)
                if (isTRUE(gzip)) {
                    matrixFile <- gzip(matrixFile, overwrite = TRUE)
                }
                # Write barcodes (colnames).
                barcodes <- colnames(counts)
                barcodesFile <- paste0(matrixFile, ".colnames")
                write_lines(barcodes, barcodesFile)
                # Write gene names (rownames).
                genes <- rownames(counts)
                genesFile <- paste0(matrixFile, ".rownames")
                write_lines(genes, genesFile)
                returnPath <- matrixFile
            } else {
                stop(paste(name, "is not a matrix"), call. = FALSE)
            }
            returnPath
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )

    invisible(files)
}
