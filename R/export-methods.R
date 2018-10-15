# FIXME Add `human = TRUE` mode, which uses sampleNames and gene symbols.



#' Export
#'
#' Export data out of R and write to disk.
#'
#' This is a wrapper for [rio::export()] that adds support for additional S4
#' classes in Bioconductor.
#'
#' @section Row names:
#'
#' The standard [rio::export()] call will drop rownames when exporting to CSV.
#' The `readr::write_*()` family of functions also never write rownames. This is
#' a *really poor* default setting for handling genomic data, which often
#' contain gene identifiers in the rownames. Here we're performing any internal
#' tibble coercion step to ensure rownames are always moved to a "`rowname`"
#' column in the CSV export.
#'
#' @name export
#' @family Import/Export Functions
#' @export
#'
#' @inheritParams general
#' @inheritParams rio::export
#' @param compress `boolean`. Apply gzip compression to all files.
#' @param human `boolean`. Automatically convert gene IDs to
#'   gene symbols in the [rownames()] and sample IDs to sample names in the
#'   [colnames()].
#'
#' @return Invisible `character`. File path(s).
#'
#' @seealso [rio::export()].
#'
#' @examples
#' data(rse_small, sce_small)
#'
#' ## matrix ====
#' rnaseq_counts <- counts(rse_small)
#' stopifnot(is.matrix(rnaseq_counts))
#' export(rnaseq_counts, format = "csv")
#'
#' ## sparseMatrix ====
#' single_cell_counts <- counts(sce_small)
#' stopifnot(is(single_cell_counts, "sparseMatrix"))
#' export(single_cell_counts, format = "mtx")
#'
#' ## Clean up.
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
            stop("Must specify `file` and/or `format`.", call. = FALSE)
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
        file <- suppressMessages(do.call(
            what = rio::export,
            args = list(x = x, file = file, ...)
        ))
        file <- realpath(file)
        message(paste0("Exported ", basename(file), "."))
        invisible(file)
    }



# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
# Consider adding HDF5 support in a future update.
.export.sparseMatrix <-  # nolint
    function(x, file, format) {
        stopifnot(is(x, "sparseMatrix"))
        assert_is_non_empty(x)
        choices <- c("mtx", "mtx.gz")

        if (missing(file) && missing(format)) {
            stop("Must specify `file` and/or `format`.", call. = FALSE)
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
        initDir(dirname(file))

        # MatrixMarket file
        writeMM(obj = x, file = file)

        if (isTRUE(gzip)) {
            file <- gzip(file, overwrite = TRUE)
        }

        # Normalize the path.
        file <- realpath(file)

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

        message(paste0("Exported ", toString(basename(files)), "."))

        # Return named character of file paths.
        invisible(files)
    }



.export.SE <-  # nolint
    function(
        x,
        dir = ".",
        compress = FALSE,
        human = FALSE
    ) {
        call <- standardizeCall()
        name <- as.character(call[["x"]])
        dir <- initDir(file.path(dir, name))
        assert_is_a_bool(compress)
        assert_is_a_bool(human)

        files <- list()

        format <- "csv"
        if (isTRUE(compress)) {
            format <- paste0(format, ".gz")
        }
        ext <- paste0(".", format)

        # Human readable data mode.
        # Ensure colnames are converted to sample names.
        # Ensure rownames are converted to gene names (symbols).
        if (isTRUE(human)) {
            files[["gene2symbol"]] <- export(
                x = gene2symbol(x),
                file = file.path(dir, paste0("gene2symbol", ext))
            )
            x <- convertGenesToSymbols(x)
            x <- convertSampleIDsToNames(x)
        }

        # Assays
        assayNames <- assayNames(x)
        stopifnot(has_length(assayNames))
        message(paste("Exporting assays:", toString(assayNames)))
        files[["assays"]] <- lapply(
            X = assayNames,
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                assay <- assays(x)[[name]]
                if (is(assay, "matrix")) {
                    format <- "csv"
                } else if (is(assay, "sparseMatrix")) {
                    format <- "mtx"
                }
                if (isTRUE(compress)) {
                    format <- paste0(format, ".gz")
                }
                file <- paste0(file, ".", format)
                export(assay, file = file)
            },
            dir = initDir(file.path(dir, "assays"))
        )
        names(files[["assays"]]) <- assayNames

        # rowData
        rowData <- rowData(x)
        if (!is.null(rowData)) {
            rownames(rowData) <- rownames(x)
            files[["rowData"]] <- export(
                x = rowData,
                file = file.path(dir, paste0("rowData", ext))
            )
        }

        # colData
        colData <- colData(x)
        if (!is.null(colData)) {
            files[["colData"]] <- export(
                x = colData,
                file = file.path(dir, paste0("colData", ext))
            )
        }

        message(paste0("Exported ", name, " to ", dir, "."))

        # Return named character of file paths.
        files <- Filter(Negate(is.null), files)
        assert_has_names(files)
        invisible(files)
    }



.export.SCE <- function(x) {
    assert_is_a_bool(compress)
    call <- standardizeCall()
    name <- as.character(call[["x"]])

    # Primarily use SE method to export.
    se <- .asSummarizedExperiment(x)
    assign(x = name, value = se)
    args <- matchArgsToDoCall()
    args[["x"]] <- as.name(name)
    files <- do.call(what = export, args = args)
    print(files)

    # Also export reducedDims to disk, if defined.
    # Note that these don't map to object rownames, so we don't have to handle
    # gene2symbol option here.
    reducedDimNames <- reducedDimNames(x)
    if (has_length(reducedDimNames)) {
        message(paste("Exporting reducedDims:", toString(reducedDimNames)))
        files[["reducedDims"]] <- lapply(
            X = reducedDimNames,
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                reducedDim <- reducedDims(x)[[name]]
                if (is(reducedDim, "matrix")) {
                    format <- "csv"
                } else if (is(reducedDim, "sparseMatrix")) {
                    format <- "mtx"
                }
                if (isTRUE(compress)) {
                    format <- paste0(format, ".gz")
                }
                file <- paste0(file, ".", format)
                export(reducedDim, file = file)
            },
            dir = initDir(file.path(dir, name, "reducedDims"))
        )
        names(files[["reducedDims"]]) <- reducedDimNames
    }

    assert_has_names(files)
    invisible(files)
}
formals(.export.SCE) <- formals(.export.SE)



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



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = .export.SCE
)
