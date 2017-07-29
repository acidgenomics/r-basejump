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
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @param ... Count matrices, passed in as dots.
#' @param dir Output directory.
#' @param gzip Compress the counts file using gzip.
#'
#' @export
writeCounts <- function(
    ...,
    dir = file.path("results", "counts"),
    gzip = TRUE) {
    dots <- list(...)
    objs <- getObjsFromDots(dots(...))

    # Create the counts output directory
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    # Iterate across the dots and write CSVs
    message(paste("Writing", toString(objs), "to", dir))
    lapply(seq_along(objs), function(a) {
        name <- objs[[a]]
        counts <- dots[[a]]
        if (is.matrix(counts) | is.data.frame(counts)) {
            # Bulk RNA-seq dense counts
            ext <- ".csv"
            if (isTRUE(gzip)) {
                ext <- paste0(ext, ".gz")
            }
            fileName <- paste0(name, ext)
            counts %>%
                as.data.frame %>%
                rownames_to_column %>%
                write_csv(path = file.path(dir, fileName))
        } else if (class(counts)[[1L]] %in% c("dgCMatrix", "dgTMatrix")) {
            # Single-cell RNA-seq sparse counts
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
                gzip(matrixFile)
            }
        } else {
            stop(paste(name, "contains an unsupported class"))
        }
    }) %>%
        invisible
}
