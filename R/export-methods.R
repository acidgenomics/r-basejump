# TODO Add automatic Tx2Gene file export for SE, if necessary.
# FIXME Ensure we're handling Rle columns properly here.



#' Export
#'
#' Export data out of R and write to disk.
#'
#' This is a wrapper for `rio::export` that adds support for additional S4
#' classes in Bioconductor.
#'
#' @section Row names:
#'
#' The standard `rio::export` call will drop rownames when exporting to CSV. The
#' readr family of functions also never write rownames. This is a *really poor*
#' default setting for handling genomic data, which often contain gene
#' identifiers in the rownames. Here we're performing any internal tibble
#' coercion step to ensure rownames are always moved to a "`rowname`" column in
#' the CSV export.
#'
#' @name export
#' @inheritParams params
#'
#' @param x An object supporting `dim` to be written to disk.
#' @param file `character(1)`. File path. Specify `file` or `format` but not
#'   both.
#' @param format `character(1)`. An optional character string containing the
#'   file format, which can be used to override the format inferred from `file`,
#'   or in lieu of specifying `file`.
#'
#' @param compress `logical(1)`. Apply gzip compression to all files.
#' @param humanize `logical(1)`. Automatically convert gene IDs to gene symbols
#'   in the `rownames` and sample IDs to sample names in the `colnames`.
#' @param name `character(1)`. Name to use on disk. If left `NULL`, will use the
#'   name of the object instead.
#' @param slotNames `character`. Names of slots to include when writing to disk.
#'
#' @return Invisible `character`. File path(s).
#'
#' @seealso `rio::export`.
#'
#' @examples
#' library(SummarizedExperiment)
#' library(SingleCellExperiment)
#' data(rse, sce)
#'
#' ## matrix ====
#' rnaseq_counts <- SummarizedExperiment::assay(rse)
#' export(rnaseq_counts, format = "csv")
#'
#' ## sparseMatrix ====
#' single_cell_counts <- SummarizedExperiment::assay(sce)
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
#' export(rse, dir = "example")
#'
#' ## SingleCellExperiment ====
#' export(sce, dir = "example")
NULL



# ANY : matrix, data.frame, DataFrame, etc. ====================================
# Coerce to tibble in this method to always preserve rownames.
# Note that `rio::export` does not preserve rownames by default.
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
        # `list_len` to `new_list`, but this isn't updated in the CRAN
        # version yet. Safe to remove the warning suppression here once this
        # is resolved.
        suppressWarnings(
            x <- as_tibble(x, rownames = rownames)
        )
        assert(hasRows(x), hasCols(x))
        if (missing(file) && missing(format)) {
            stop("Must specify `file` and/or `format`.", call. = FALSE)
        } else if (missing(file)) {
            call <- standardizeCall()
            sym <- call[["x"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            assert(isString(format))
            file <- paste0(name, ".", format)
        } else if (missing(format)) {
            assert(isString(file))
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



# sparseMatrix =================================================================
# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
# Consider adding HDF5 support in a future update.
export.sparseMatrix <-  # nolint
    function(x, file, format) {
        assert(hasLength(x))
        choices <- c("mtx", "mtx.gz")

        if (missing(file) && missing(format)) {
            stop("Must specify `file` and/or `format`.", call. = FALSE)
        } else if (missing(file)) {
            call <- standardizeCall()
            sym <- call[["x"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            format <- match.arg(format, choices)
            file <- paste0(name, ".", format)
        } else if (missing(format)) {
            assert(isString(file))
            # Require a valid extension.
            grepChoices <- paste0("\\.", choices, "$")
            assert(any(vapply(
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



# SummarizedExperiment =========================================================
.export.assays <-  # nolint
    function(
        x,
        name,
        dir,
        compress,
        humanize
    ) {
        assayNames <- assayNames(x)
        assert(isCharacter(assayNames))
        message(paste("Exporting assays:", toString(assayNames)))
        if (isTRUE(humanize)) {
            g2s <- Gene2Symbol(x)
        } else {
            g2s <- NULL
        }
        out <- lapply(
            X = assayNames,
            FUN = function(name, dir, g2s) {
                file <- file.path(dir, name)
                assay <- assays(x)[[name]]

                # For humanize mode, coerce to data frame and cbind the
                # gene-to-symbol mappings to the left side.
                if (!is.null(g2s)) {
                    message("Adding `geneID` and `geneName` columns.")
                    assay <- as(assay, "DataFrame")
                    assert(
                        identical(rownames(assay), rownames(g2s)),
                        areDisjointSets(colnames(assay), colnames(g2s))
                    )
                    assay <- cbind(g2s, assay)
                    rownames(assay) <- NULL
                }

                # Dynamically set the file name based on the assay type.
                if (is(assay, "sparseMatrix")) {
                    format <- "mtx"
                } else {
                    format <- "csv"
                }
                if (isTRUE(compress)) {
                    format <- paste0(format, ".gz")
                }
                file <- paste0(file, ".", format)

                export(assay, file = file)
            },
            dir = initDir(file.path(dir, "assays")),
            g2s = g2s
        )
        names(out) <- assayNames
        out
    }



# NOTE This step will break if `rowData` doesn't contain `entrezID`.
.export.Ensembl2Entrez <-  # nolint
    function(x, ext, dir) {
        export(
            x = Ensembl2Entrez(x),
            file = file.path(dir, paste0("Ensembl2Entrez", ext))
        )
    }



.export.Gene2Symbol <-  # nolint
    function(x, ext, dir) {
        export(
            x = Gene2Symbol(x),
            file = file.path(dir, paste0("Gene2Symbol", ext))
        )
    }



.export.colData <-  # nolint
    function(x, ext, dir) {
        export(
            x = sanitizeColData(colData(x)),
            file = file.path(dir, paste0("colData", ext))
        )
    }



# NOTE: The standard `rowData` output is okay but doesn't include genomic
# ranges coordinates. That's why we're coercing from `rowRanges` for RSE.
.export.rowData <-  # nolint
    function(x, ext, dir) {
        if (is(x, "RangedSummarizedExperiment")) {
            data <- rowRanges(x)
        } else {
            data <- rowData(x)
        }
        data <- sanitizeRowData(data)
        assert(identical(rownames(data), rownames(x)))
        export(x = data, file = file.path(dir, paste0("rowData", ext)))
    }



# Note that for humanize mode, we're outputting both the `geneID` and `geneName`
# columns per matrix. This requires gene-to-symbol mappings to be defined in
# `rowData`, otherwise the function will intentionally error. In this case,
# we're also consistently not using a "rowname" column in the output.
# TODO Include 1:1 Ensembl2Entrez mappings in output.
export.SummarizedExperiment <-  # nolint
    function(
        x,
        name = NULL,
        dir = ".",
        compress = FALSE,
        humanize = FALSE,
        slotNames = c(
            "assays",
            "colData",
            "rowData",
            "Gene2Symbol",
            "Ensembl2Entrez"
        )
    ) {
        call <- standardizeCall()
        assert(isString(name) || is.null(name))
        if (is.null(name)) {
            name <- as.character(call[["x"]])
        }
        dir <- initDir(file.path(dir, name))
        assert(
            isFlag(compress),
            isFlag(humanize),
            isCharacter(slotNames),
            # Require at least 1 of the slotNames to be defined for export.
            # Note that we're not using `match.arg` here.
            isSubset(x = slotNames, y = eval(formals()[["slotNames"]]))
        )

        # Return the file paths back to the user as a named list.
        files <- list()

        # Set the desired output extension, depending on whether we need to
        # enable compression. Always output consistently in CSV format.
        format <- "csv"
        if (isTRUE(compress)) {
            format <- paste0(format, ".gz")
        }
        ext <- paste0(".", format)

        # Ensure the assays list is always named. Note that valid SE objects
        # don't have to contain named assays (e.g. DESeqTransform). In the event
        # that an SE object contains a single, unnamed assay, we make sure to
        # rename it internally to "assay" before exporting.
        if (is.null(assayNames(x))) {
            # Be sure to run this step before attempting `humanize` call for
            # `humanize = TRUE`, because the dimnames won't necessarily be
            # valid, and the `assayNames<-` function is strict.
            assayNames(x) <- "assay"
        }

        # Human readable data mode (humanize).
        # Ensure colnames are converted to sample names.
        # Ensure rownames are converted to gene names (symbols).
        # Run this step last on SE objects because the dimnames will no longer
        # be valid, and this can cause other SE methods to error.
        if (isTRUE(humanize)) {
            x <- humanize(x)
        }

        # Assays (count matrices). Note that for humanize mode, we're coercing
        # to a data frame and ensuring that both geneID and geneName columns
        # are defined at the beginning. This makes re-import of the data a
        # little more tricky because we no longer have a single rowname column,
        # but it is a lot easier for non-bioinformaticians to work with.
        if ("assays" %in% slotNames) {
            files[["assays"]] <-
                .export.assays(
                    x = x,
                    name = name,
                    dir = dir,
                    compress = compress,
                    humanize = humanize
                )
        }

        # Column annotations.
        if ("colData" %in% slotNames) {
            files[["colData"]] <-
                .export.colData(x = x, ext = ext, dir = dir)
        }

        # Row annotations.
        if ("rowData" %in% slotNames) {
            files[["rowData"]] <-
                .export.rowData(x = x, ext = ext, dir = dir)
        }
        if ("Gene2Symbol" %in% slotNames) {
            files[["Gene2Symbol"]] <- tryCatch(
                expr = .export.Gene2Symbol(x = x, ext = ext, dir = dir),
                error = function(e) {
                    warning("Failed to export gene-to-symbol mappings.")
                }
            )
        }
        if ("Ensembl2Entrez" %in% slotNames) {
            # Objects generated using GFF/GTF won't contain this.
            files[["Ensembl2Entrez"]] <- tryCatch(
                expr = .export.Ensembl2Entrez(x = x, ext = ext, dir = dir),
                error = function(e) {
                    warning("Failed to export Ensembl-to-Entrez mappings.")
                    NULL
                }
            )
        }

        message(paste0("Exported ", name, " to ", dir, "."))

        # Return named character of file paths.
        files <- Filter(Negate(is.null), files)
        assert(hasNames(files))
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = export.SummarizedExperiment
)



# FIXME Need to define an export sampleData step here.
export.SingleCellExperiment <-  # nolint
    function(x) {
        assert(isFlag(compress))
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
        # humanize mode is enabled, because the rownames map to cells, and the
        # colnames map to the dimensions of interest.
        reducedDimNames <- reducedDimNames(x)
        if (length(reducedDimNames) == 0L) {
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

        assert(hasNames(files))
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
