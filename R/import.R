#' Import
#'
#' Read file by extension into R.
#'
#' This is a wrapper for [rio::import()] that adds support for additional common
#' genomic data files. Remote URLs and compressed files are supported. All
#' extensions are case insensitive.
#'
#' This function supports automatic loading of common file types:
#'
#' - `CSV`: Comma Separated Values.
#' - `TSV` Tab Separated Values.
#' - `XLSX`: Excel workbook.
#' - `MTV`: MatrixMarket sparse matrix.
#' - `GTF`/`GFF`/`GFF3`: General Feature Format.
#' - `RDA`/`RDATA`: R Data.
#'     - Must contain a single object.
#'     - Doesn't require internal object name to match, unlike [loadData()].
#' - `RDS`: R Data Serialized.
#' - `JSON`: JSON.
#' - `YAML`/`YML`: YAML.
#'
#' These file formats will be read as (source code) lines:
#' `LOG`, `MD`, `PY`, `R`, `RMD`, `SH`.
#'
#' These file formats are blacklisted, and intentionally not supported:
#' `DOC`, `DOCX`, `PDF`, `PPT`, `PPTX`, `TXT`.
#'
#' If the file format isn't supported natively (or blacklisted), the
#' [rio](https://cran.r-project.org/web/packages/rio/index.html) package will
#' be used as a fallback attempt.
#'
#' @section Matrix Market Exchange (MTX/MEX):
#'
#' Reading a Matrix Market Exchange (`MTX`) file now requires `COLNAMES` and
#' `ROWNAMES` sidecar files containing the [colnames()] and [rownames()] of
#' the sparse matrix. Legacy support for manual loading of these sidecar files
#' is provided.
#'
#' @section General Feature Format (GFF/GTF):
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' Column names follow the [Ensembl conventions](https://bit.ly/2K6EBla).
#
#' Additional information:
#'
#' - [Ensembl](http://www.ensembl.org/info/website/upload/gff.html)
#' - [Gencode](http://www.gencodegenes.org/gencodeformat.html)
#'
#' @section bcbio:
#'
#' Also supports some additional extensions commonly used with the
#' [bcbio](https://bcbio-nextgen.readthedocs.io) pipeline:
#'
#' - `COUNTS`: Counts table (e.g. RNA-seq aligned counts).
#' - `COLNAMES`: Sidecar file containing column names.
#' - `ROWNAMES`: Sidecar file containing row names.
#'
#' @family Import/Export Functions
#' @export
#'
#' @inheritParams general
#'
#' @return Varies, depending on the file extension:
#'
#' - Default: `DataFrame`.
#' - `MTX`: `sparseMatrix`.
#' - `GTF`/`GFF`: `GRanges`.
#' - `JSON`/`YAML`: `list`.
#' - Source code or log: `character`.
#'
#' Note that data frames are returned as S4 `DataFrame` class instead of
#' `data.frame`, when applicable.
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix](https://cran.r-project.org/web/packages/Matrix/index.html).
#'
#' @examples
#' ## R Data
#' x <- import(file = file.path(basejumpCacheURL, "rnaseq_counts.rda"))
#' class(x)
#'
#' ## Comma Separated Values
#' x <- import(file = file.path(basejumpCacheURL, "example.csv"))
#' print(x)
#'
#' ## Microsoft Excel Worksheet
#' x <- import(file = file.path(basejumpCacheURL, "example.xlsx"))
#' print(x)
#'
#' ## GTF/GFF
#' x <- import(file = file.path(basejumpCacheURL, "example.gtf"))
#' summary(x)
#'
#' ## JSON
#' x <- import(file = file.path(basejumpCacheURL, "example.json"))
#' names(x)
#'
#' ## YAML
#' x <- import(file = file.path(basejumpCacheURL, "example.yml"))
#' names(x)
#'
#' ## Counts Table (i.e. aligned counts from bcbio)
#' x <- import(file = file.path(basejumpCacheURL, "example.counts"))
#' colSums(x)
import <- function(file, ...) {
    args <- list(file, ...)
    assert_is_a_string(file)

    message(paste0("Reading ", basename(file), "."))
    file <- localOrRemoteFile(file)

    # Note that matching is case insensitive.
    ext <- basename(file) %>%
        str_match(extPattern) %>%
        .[1L, 2L] %>%
        toupper()

    blacklist <- c("DOC", "DOCX", "PDF", "PPT", "PPTX", "TXT")
    lines <- c("LOG", "MD", "PY", "R", "RMD", "SH")
    unsupported <- paste("Unsupported extension", ":", deparse(ext))

    if (ext %in% blacklist) {
        stop(unsupported)
    } else if (ext %in% lines) {
        message("Importing using `read_lines()`.")
        data <- do.call(what = read_lines, args = args)
    } else if (ext %in% c("COLNAMES", "ROWNAMES")) {
        args[["na"]] <- na
        data <- do.call(what = read_lines, args = args)
    } else if (ext == "COUNTS") {
        # bcbio counts output.
        args[["na"]] <- na
        data <- do.call(what = read_tsv, args = args)
        assert_is_subset("id", colnames(data))

        # Remove duplicate genes in the pseudo-autosomal region (PAR), when
        # loading counts from GENCODE.
        pattern <- "_PAR_[[:upper:]]$"
        if (any(grepl(pattern, data[["id"]]))) {
            message(paste(
                "Removed duplicate genes in the pseudo-autosomal region (PAR)."
            ))
            keep <- !grepl(pattern, data[["id"]])
            data <- data[keep, , drop = FALSE]
        }

        # Ensure transcript versions are stripped.
        data[["id"]] <- stripTranscriptVersions(data[["id"]])

        # Coerce to matrix.
        assert_has_no_duplicates(data[["id"]])
        data <- data %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else if (ext %in% c("GFF", "GFF3", "GTF")) {
        data <- .importGFF(file)
    } else if (ext == "MTX") {
        # MatrixMarket
        # Require `.ROWNAMES` and `.COLNAMES` files.
        data <- do.call(what = readMM, args = args)
        rownames <- paste(file, "rownames", sep = ".") %>%
            localOrRemoteFile() %>%
            read_lines(na = na)
        colnames <- paste(file, "colnames", sep = ".") %>%
            localOrRemoteFile() %>%
            read_lines(na = na)
        rownames(data) <- rownames
        colnames(data) <- colnames
    } else if (ext %in% c("RDA", "RDATA")) {
        safe <- new.env()
        object <- load(file, envir = safe)
        if (!has_length(safe, n = 1L)) {
            stop("File does not contain a single object.")
        }
        data <- get(object, envir = safe, inherits = FALSE)
    } else if (ext == "RDS") {
        data <- readRDS(file)
    } else if (ext == "JSON") {
        data <- .importJSON(file)
    } else if (ext %in% c("YAML", "YML")) {
        data <- .importYAML(file)
    } else {
        # `rio::import`
        # How we declare NA strings depends on the file extension.
        if (ext %in% c("CSV", "TSV")) {
            # `data.table::fread`
            args[["na.strings"]] <- na
        } else if (ext %in% c("XLS", "XLSX")) {
            # `readxl::read_excel`
            args[["na"]] <- na
        }
        data <- do.call(what = rio::import, args = args)
    }

    # Return as `DataFrame` instead of `data.frame`.
    if (is.data.frame(data)) {
        data <- as(data, "DataFrame")
        if ("rowname" %in% colnames(data)) {
            rownames(data) <- data[["rowname"]]
            data[["rowname"]] <- NULL
        }
        # Ensure that rownames assign correctly, if necessary.
        assert_are_disjoint_sets("rowname", colnames(data))
    }

    data
}



.importGFF <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(
        x = toupper(basename(file)),
        pattern = "\\.G(F|T)F([[:digit:]])?(\\.GZ)?$"
    )
    file <- localOrRemoteFile(file)
    tryCatch(
        rtracklayer::import(file),
        error = function(e) {
            stop("GFF file failed to load.")  # nocov
        },
        warning = function(w) {
            stop("GFF file failed to load.")  # nocov
        }
    )
}



.importJSON <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(
        x = toupper(basename(file)),
        pattern = "\\.JSON$"
    )
    file <- localOrRemoteFile(file)
    read_json(file)
}



.importYAML <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(
        x = toupper(basename(file)),
        pattern = "\\.YA?ML$"
    )
    file <- localOrRemoteFile(file)
    yaml.load_file(file)
}
