#' Write counts
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
    # FIXME Switch to tidy dots method
    names <- as.character(substitute(list(...)))[-1L]
    dots <- list(...)

    # Create the counts output directory
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    # Iterate across the dots and write CSVs
    message(paste("Writing", toString(names), "to", dir))
    lapply(seq_along(names), function(a) {
        name <- names[a]
        counts <- dots[a]
        if (is.data.frame(counts)) {
            # Bulk RNA-seq dense counts
            ext <- ".csv"
            if (isTRUE(gzip)) {
                ext <- str_c(ext, ".gz")
            }
            file_name <- str_c(name, ".", ext)
            counts %>%
                as.data.frame %>%
                rownames_to_column %>%
                write_csv(path = file.path(dir, file_name))
        } else if (class(counts)[[1L]] %in% c("dgCMatrix", "dgTMatrix")) {
            # Single-cell RNA-seq sparse counts
            matrix_file <- file.path(dir, str_c(name, ".mtx"))
            writeMM(counts, matrix_file)

            # Write barcodes (colnames)
            barcodes <- colnames(counts)
            barcodes_file <- str_c(matrix_file, ".colnames")
            write_lines(barcodes, barcodes_file)

            # Write gene names (rownames)
            genes <- rownames(counts)
            genes_file <- str_c(matrix_file, ".rownames")
            write_lines(genes, genes_file)

            # gzip the matrix, if desired
            if (isTRUE(gzip)) {
                gzip(matrix_file)
            }
        } else {
            stop(paste(name, "contains an unsupported class"))
        }
    }) %>% invisible
}

#' @rdname writeCounts
#' @usage NULL
#' @export
writeCounts -> write_counts  # nolint
