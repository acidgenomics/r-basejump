#' Export
#'
#' Export data out of R and write to disk.
#'
#' This is a wrapper for [rio::export()] that adds support for additional S4
#' classes in Bioconductor.
#'
#' @note The standard [rio::export()] call will drop rownames when exporting to
#'   CSV. We're performing any internal tibble coercion step to ensure rownames
#'   are always moved to a "rowname" column in the CSV output.
#'
#' @name export
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @inheritParams rio::export
#'
#' @return Invisible `character`. File path(s).
#'
#' @seealso [rio::export()].
#'
#' @examples
#' rnaseq_counts <- counts(rse_small)
#' single_cell_counts <- counts(sce_small)
#'
#' export(rnaseq_counts, format = "csv")
#' export(single_cell_counts, format = "mtx")
#'
#' # Clean up
#' file.remove(c(
#'     "rnaseq_counts.csv",
#'     "single_cell_counts.mtx",
#'     "single_cell_counts.mtx.colnames",
#'     "single_cell_counts.mtx.rownames"
#' ))
NULL



# Coerce to tibble in this method to always preserve rownames.
# Note that `rio::export()` does not preserve rownames by default.
.export.ANY <-  # nolint
    function(x, file, format, ...) {
        assert_is_non_empty(x)
        # Ensure rownames are automatically moved to `rowname` column.
        if (hasRownames(x)) {
            rownames <- "rowname"
        } else {
            rownames <- NULL
        }
        # Coercing to tibble to provide consistent write support.
        x <- as_tibble(x, rownames = rownames)
        if (missing(file) && missing(format)) {
            stop("Must specify 'file' and/or 'format'", call. = FALSE)
        } else if (missing(file)) {
            call <- standardizeCall()
            sym <- call[["x"]]
            assert_is_symbol(sym)
            name <- as.character(sym)
            assert_is_a_string(format)
            file <- paste0(name, ".", format)
        } else if (missing(format)) {
            assert_is_a_string(file)
        }
        file <- do.call(
            what = rio::export,
            args = list(x = x, file = file, ...)
        )
        file <- normalizePath(file, winslash = "/", mustWork = TRUE)
        message(paste("Exported", basename(file)))
        invisible(file)
    }



# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
# Consider adding HDF5 support in a future update.
.export.sparseMatrix <-  # nolint
    function(x, file, format) {
        assert_is_non_empty(x)
        choices <- c("mtx", "mtx.gz")

        if (missing(file) && missing(format)) {
            stop("Must specify 'file' and/or 'format'", call. = FALSE)
        } else if (missing(file)) {
            call <- standardizeCall()
            sym <- call[["x"]]
            assert_is_symbol(sym)
            name <- as.character(sym)
            format <- match.arg(format, choices)
            file <- paste0(name, ".", format)
        } else if (missing(format)) {
            assert_is_a_string(file)
            # Require a valid extension.
            grepChoices <- paste0("\\.", choices, "$")
            stopifnot(any(vapply(
                X = grepChoices,
                FUN = grepl,
                FUN.VALUE = logical(1L),
                x = file
            )))
        }

        # Determine whether we want to gzip compress.
        gzip <- grepl("\\.gz$", file)

        # Now ensure ".gz" is stripped from the working file variable.
        file <- sub("\\.gz", "", file)

        # Create the recursive directory structure, if necessary.
        initializeDirectory(dirname(file))

        # MatrixMarket file
        writeMM(obj = x, file = file)

        if (isTRUE(gzip)) {
            file <- gzip(file, overwrite = TRUE)
        }

        # Normalize the path.
        file <- normalizePath(file, winslash = "/", mustWork = TRUE)

        # Write barcodes (colnames).
        barcodes <- colnames(x)
        barcodesFile <- paste0(file, ".colnames")
        write_lines(x = barcodes, path = barcodesFile)

        # Write gene names (rownames).
        genes <- rownames(x)
        genesFile <- paste0(file, ".rownames")
        write_lines(x = genes, path = genesFile)

        files <- c(
            matrix = file,
            barcodes = barcodesFile,
            genes = genesFile
        )

        message(paste("Exported", toString(basename(files))))

        # Return named character of file paths.
        invisible(files)
    }



.export.SE <-  # nolint
    function(x, dir = ".") {
        call <- standardizeCall()
        name <- as.character(call[["x"]])
        dir <- initializeDirectory(file.path(dir, name))
        message(paste("Exporting", name, "to", dir))

        # Assays
        assays <- lapply(
            X = assayNames(x),
            FUN = function(assayName) {
                dir <- initializeDirectory(file.path(dir, "assays"))
                file <- file.path(dir, assayName)
                assay <- assays(x)[[assayName]]
                if (is(assay, "matrix")) {
                    format <- "csv.gz"
                } else if (is(assay, "sparseMatrix")) {
                    format <- "mtx.gz"
                }
                file <- paste0(file, ".", format)
                export(assay, file = file)
            }
        )

        # rowData
        rowData <- rowData(x)
        if (!is.null(rowData)) {
            rownames(rowData) <- rownames(x)
            rowData <- export(rowData, file = file.path(dir, "rowData.csv"))
        }

        # colData
        colData <- colData(x)
        if (!is.null(colData)) {
            colData <- export(colData, file = file.path(dir, "colData.csv"))
        }

        files <- list(
            assays = assays,
            rowData = rowData,
            colData = colData
        )
        files <- Filter(Negate(is.null), files)

        # Return named character of file paths.
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("ANY"),
    definition = .export.ANY
)



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("sparseMatrix"),
    definition = .export.sparseMatrix
)



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = .export.SE
)
