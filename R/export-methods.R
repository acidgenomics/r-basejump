#' Export
#'
#' Export data out of R and write to disk.
#'
#' This is a wrapper for [rio::export()] that adds support for additional S4
#' classes in Bioconductor.
#'
#' @name export
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams rio::export
#' @param x Object.
#'
#' @seealso [rio::export()].
#'
#' @examples
#' export(mtcars, format = "csv")
NULL



.export.ANY <-  # nolint
function(x, file, format, ...) {
    assert_is_non_empty(x)
    call <- standardizeCall()
    sym <- call[["x"]]
    assert_is_symbol(sym)
    name <- as.character(sym)
    if (missing(file) && missing(format)) {
        stop("Must specify 'file' and/or 'format'")
    } else if (missing(file)) {
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
    message(paste("Exported", name, "to", file))
    file
}



# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
# Consider adding HDF5 support in a future update.
.export.sparseMatrix <-  # nolint
function(x, file, format) {
    assert_is_non_empty(x)
    # FIXME This is breaking
    # call <- standardizeCall()
    call <-
    print(call)
    stop("HELLO THERE")
    sym <- call[["x"]]
    assert_is_symbol(sym)
    name <- as.character(sym)
    choices = c("mtx", "mtx.gz")

    if (missing(file)) {
        format <- match.arg(format, choices)
        file <- paste0(name, ".", format)
    } else {
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

    message(paste("Exported", name, "to", file))

    # Return named character of file paths.
    c(
        matrix = file,
        barcodes = barcodesFile,
        genes = genesFile
    )
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
