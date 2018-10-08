#' Import
#'
#' Read file by extension into R.
#'
#' This is a wrapper for [rio::import()] that adds support for additional common
#' genomic data files. Remote URLs and compressed files are supported.
#'
#' This function supports automatic loading of common file types:
#'
#' - `csv`: Comma Separated Values.
#' - `tsv` Tab Separated Values.
#' - `xlsx`: Excel workbook.
#' - `mtx`: MatrixMarket sparse matrix.
#' - `gtf`/`gff`/`gff3`: General Feature Format.
#' - `rda`/`RData`: R Data. Must contain a single object. Doesn't require
#'   internal object name to match, like with the [loadData()] function.
#' - `rds`: R Data Serialized.
#' - `json`: JSON.
#' - `yaml`/`yml`: YAML.
#'
#' If the file format isn't supported natively (or blacklisted), the
#' [rio](https://cran.r-project.org/web/packages/rio/index.html) package will
#' be used as a fallback attempt.
#'
#' @section Matrix Market Exchange (MTX/MEX):
#'
#' Reading a Matrix Market Exchange ("`mtx`") file now requires "`colnames`" and
#' `"rownames"` sidecar files containing the [colnames()] and [rownames()] of
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
#' - `counts`: Counts table (e.g. RNA-seq aligned counts).
#' - `colnames`: Sidecar file containing column names.
#' - `rownames`: Sidecar file containing row names.
#'
#' @family Import/Export Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return Varies, depending on the file extension:
#'
#' - Default: `DataFrame`.
#' - MTX: `sparseMatrix`.
#' - GTF/GFF: `GRanges`.
#' - JSON/YAML: `list`.
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
#' # R Data
#' x <- import(file = file.path(basejumpCacheURL, "rnaseq_counts.rda"))
#' class(x)
#'
#' # Comma Separated Values
#' x <- import(file = file.path(basejumpCacheURL, "example.csv"))
#' print(x)
#'
#' # Microsoft Excel Worksheet
#' x <- import(file = file.path(basejumpCacheURL, "example.xlsx"))
#' print(x)
#'
#' # GTF/GFF
#' x <- import(file = file.path(basejumpCacheURL, "example.gtf"))
#' summary(x)
#'
#' # JSON
#' x <- import(file = file.path(basejumpCacheURL, "example.json"))
#' names(x)
#'
#' # YAML
#' x <- import(file = file.path(basejumpCacheURL, "example.yml"))
#' names(x)
#'
#' # Counts Table (i.e. aligned counts from bcbio)
#' x <- import(file = file.path(basejumpCacheURL, "example.counts"))
#' colSums(x)
import <- function(file, ...) {
    args <- list(file, ...)
    assert_is_a_string(file)

    message(paste0("Reading ", basename(file), "..."))
    file <- localOrRemoteFile(file)

    ext <- basename(file) %>%
        str_match(extPattern) %>%
        .[1L, 2L] %>%
        tolower()

    blacklist <- c("doc", "docx", "ppt", "pptx", "txt")
    source <- c("md", "py", "r", "rmd", "sh")
    unsupported <- paste("Unsupported extension", ":", deparse(ext))

    if (ext %in% blacklist) {
        stop(unsupported)  # nocov
    } else if (ext %in% source) {
        message("Importing as source code lines.")
        data <- do.call(what = read_lines, args = args)
    } else if (ext %in% c("colnames", "rownames")) {
        args[["na"]] <- na
        data <- do.call(what = read_lines, args = args)
    } else if (ext == "counts") {
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
    } else if (ext %in% c("gff", "gff3", "gtf")) {
        data <- .importGFF(file)
    } else if (ext == "mtx") {
        # MatrixMarket
        # Require `.rownames` and `.colnames` files.
        data <- do.call(what = readMM, args = args)
        rownames <- paste(file, "rownames", sep = ".") %>%
            localOrRemoteFile() %>%
            read_lines(na = na)
        colnames <- paste(file, "colnames", sep = ".") %>%
            localOrRemoteFile() %>%
            read_lines(na = na)
        rownames(data) <- rownames
        colnames(data) <- colnames
    } else if (ext %in% c("rda", "rdata")) {
        safe <- new.env()
        object <- load(file, envir = safe)
        if (!has_length(safe, n = 1L)) {
            stop("File does not contain a single object.")
        }
        data <- get(object, envir = safe, inherits = FALSE)
    } else if (ext == "rds") {
        data <- readRDS(file)
    } else if (ext == "json") {
        data <- .importJSON(file)
    } else if (ext %in% c("yaml", "yml")) {
        data <- .importYAML(file)
    } else {
        # `rio::import`
        # How we declare NA strings depends on the file extension.
        if (ext %in% c("csv", "tsv")) {
            # `data.table::fread`
            args[["na.strings"]] <- na
        } else if (ext %in% c("xls", "xlsx")) {
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
    assert_all_are_matching_regex(file, "\\.g(f|t)f(\\d)?(\\.gz)?$")
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
    assert_all_are_matching_regex(file, "\\.json$")
    file <- localOrRemoteFile(file)
    read_json(file)
}



.importYAML <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.ya?ml$")
    file <- localOrRemoteFile(file)
    yaml.load_file(file)
}
