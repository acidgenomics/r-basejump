#' Export
#'
#' Export data out of R and write to disk.
#'
#' This is a wrapper for `rio::export()` that adds support for additional S4
#' classes in Bioconductor.
#'
#' @section Row names:
#'
#' The standard `rio::export()` call will drop rownames when exporting to CSV.
#' The `readr::write_*()` family of functions also never write rownames. This is
#' a *really poor* default setting for handling genomic data, which often
#' contain gene identifiers in the rownames. Here we're performing any internal
#' tibble coercion step to ensure rownames are always moved to a "`rowname`"
#' column in the CSV export.
#'
#' @name export
#' @inheritParams params
#' @inheritParams rio::export
#'
#' @param compress `boolean`. Apply gzip compression to all files.
#' @param human `boolean`. Automatically convert gene IDs to gene symbols in the
#'   `rownames()` and sample IDs to sample names in the `colnames()`.
#' @param name `string`. Name to use on disk. If left `NULL`, will use the name
#'   of the object instead.
#' @param slotNames `character`. Names of slots to include when writing to disk.
#'
#' @return Invisible `character`. File path(s).
#'
#' @seealso `rio::export()`.
#'
#' @examples
#' library(SummarizedExperiment)
#' library(SingleCellExperiment)
#' data(rse, sce)
#'
#' ## matrix ====
#' rnaseq_counts <- assay(rse)
#' export(rnaseq_counts, format = "csv")
#'
#' ## sparseMatrix ====
#' single_cell_counts <- assay(sce)
#' export(single_cell_counts, format = "mtx")
#'
#' ## Clean up.
#' file.remove(c(
#'     "rnaseq_counts.csv",
#'     "single_cell_counts.mtx",
#'     "single_cell_counts.mtx.colnames",
#'     "single_cell_counts.mtx.rownames"
#' ))
#'
#' ## SummarizedExperiment ====
#' export(rse, dir = tempdir())
#'
#' ## SingleCellExperiment ====
#' export(sce, dir = tempdir())
NULL



# Coerce to tibble in this method to always preserve rownames.
# Note that `rio::export()` does not preserve rownames by default.
export.ANY <-  # nolint
    function(x, file, format, ...) {
        # Ensure rownames are automatically moved to `rowname` column.
        if (hasRownames(x)) {
            rownames <- "rowname"
        } else {
            rownames <- NULL
        }
        # Coercing to tibble to provide consistent write support.
        # Note that tibble is warning about rlang v0.3 soft deprecation of
        # `list_len()` to `new_list()`, but this isn't updated in the CRAN
        # version yet. Safe to remove the warning suppression here once this
        # is resolved.
        suppressWarnings(
            x <- as_tibble(x, rownames = rownames)
        )
        assert_has_rows(x)
        assert_has_cols(x)
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
        # Ensure directory is created automatically.
        initDir(dir = dirname(file))
        requireNamespace("rio")
        suppressMessages(
            file <- do.call(
                what = rio::export,
                args = list(x = x, file = file, ...)
            )
        )
        file <- realpath(file)
        message(paste0("Exported ", basename(file), "."))
        invisible(file)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("ANY"),
    definition = export.ANY
)



# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
# Consider adding HDF5 support in a future update.
export.sparseMatrix <-  # nolint
    function(x, file, format) {
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
            assert_that(any(vapply(
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

        # MatrixMarket file.
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



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("sparseMatrix"),
    definition = export.sparseMatrix
)



export.SummarizedExperiment <-  # nolint
    function(
        x,
        name = NULL,
        dir = ".",
        compress = FALSE,
        human = FALSE,
        slotNames = c("assays", "colData", "rowData")
    ) {
        call <- standardizeCall()
        assertIsStringOrNULL(name)
        if (is.null(name)) {
            name <- as.character(call[["x"]])
        }
        dir <- initDir(file.path(dir, name))
        assert_is_a_bool(compress)
        assert_is_a_bool(human)
        # Require at least 1 of the slotNames to be defined for export.
        # Note that we're not using `match.arg()` here.
        assert_is_character(slotNames)
        assert_is_subset(slotNames, c("assays", "rowData", "colData"))

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
            x <- humanize(x)
        }

        # Assays (count matrices).
        if ("assays" %in% slotNames) {
            # Note that valid SE objects don't have to contain named assays
            # (e.g. DESeqTransform). In the event that an SE object contains a
            # single, unnamed assay, we make sure to rename it internally to
            # "assay" before exporting.
            if (is.null(assayNames(x))) {
                assayNames(x) <- "assay"
            }
            assayNames <- assayNames(x)
            assert_is_character(assayNames)
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
        }

        # Row annotations.
        if ("rowData" %in% slotNames) {
            rowData <- rowData(x)
            if (!is.null(rowData)) {
                rownames(rowData) <- rownames(x)
                files[["rowData"]] <- export(
                    x = rowData,
                    file = file.path(dir, paste0("rowData", ext))
                )
            }
        }

        # Column annotations.
        if ("colData" %in% slotNames) {
            colData <- colData(x)
            if (!is.null(colData)) {
                files[["colData"]] <- export(
                    x = colData,
                    file = file.path(dir, paste0("colData", ext))
                )
            }
        }

        message(paste0("Exported ", name, " to ", dir, "."))

        # Return named character of file paths.
        files <- Filter(Negate(is.null), files)
        assert_has_names(files)
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = export.SummarizedExperiment
)



export.SingleCellExperiment <-  # nolint
    function(x) {
        assert_is_a_bool(compress)
        call <- standardizeCall()
        name <- as.character(call[["x"]])

        # Primarily use SE method to export.
        se <- as(x, "RangedSummarizedExperiment")

        assign(x = name, value = se)
        args <- matchArgsToDoCall()
        args[["x"]] <- as.name(name)
        files <- do.call(what = export, args = args)
        print(files)

        # Note that we don't have to worry about sanizing reduced dims when
        # human mode is enabled, because the rownames map to cells, and the
        # colnames map to the dimensions of interest.
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

formals(export.SingleCellExperiment) <- formals(export.SummarizedExperiment)



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = export.SingleCellExperiment
)
