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
function(x, ...) {
    assert_is_non_empty(x)
    call <- standardizeCall()
    sym <- call[["x"]]
    assert_is_symbol(sym)
    name <- as.character(sym)
    file <- do.call(what = rio::export, args = as.list(call)[-1L])
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    message(paste("Exported", name, "to", file))
    file
}

# Assign the formals.
formals(.export.ANY) <- formals(rio::export)



# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
.export.sparseMatrix <-  # nolint
function(x, file, format) {
    call <- standardizeCall()
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

    message(paste("Exporting", name, "to", file))

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
    barcodes <- colnames(counts)
    barcodesFile <- paste0(file, ".colnames")
    write_lines(x = barcodes, path = barcodesFile)

    # Write gene names (rownames).
    genes <- rownames(counts)
    genesFile <- paste0(file, ".rownames")
    write_lines(x = genes, path = genesFile)

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



# FIXME Handle gzip
#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("sparseMatrix"),
    definition = .export.sparseMatrix
)
