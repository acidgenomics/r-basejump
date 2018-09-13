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
    x <- getGenerics(where = parent.frame())
    print(length(x))
    print(sys.status())
    stop()
    call <- matchCall(verbose = TRUE)
    print(call)
    stop()

    sym <- call[["x"]]
    assertive::assert_is_symbol(sym)
    message(paste("Exporting", sym, "to", file))
    do.call(
        what = rio::export,
        args = as.list(call)[-1L]
    )
}

# Assign the formals.
formals(.export.ANY) <- formals(rio::export)



.export.sparseMatrix <-  # nolint
function(x, file, format) {
    call <- matchCall()
    name <- call[["x"]]
    print(str(name))
    assert_is_name(name)
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

    # Ensure ".gz" is stripped from the working file variable.
    file <- sub("\\.gz", "", file)

    # Create the recursive directory structure, if necessary.
    initializeDirectory(dirname(file))

    # MatrixMarket file
    writeMM(obj = x, file = file)

    if (isTRUE(gzip)) {
        file <- gzip(file, overwrite = TRUE)
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
