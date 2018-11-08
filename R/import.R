# FIXME Excel files contaning integer column names (e.g. 1, 2, 3) will import
# as 1.0, 2.0, 3.0. Need to fix this behavior.



#' Import
#'
#' Read file by extension into R.
#'
#' This is a wrapper for `rio::import()` that adds support for additional common
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
#' @section bcbio files:
#'
#' Also supports some additional extensions commonly used with the
#' [bcbio](https://bcbio-nextgen.readthedocs.io) pipeline:
#'
#' - `COUNTS`: Counts table (e.g. RNA-seq aligned counts).
#' - `COLNAMES`: Sidecar file containing column names.
#' - `ROWNAMES`: Sidecar file containing row names.
#'
#' @export
#'
#' @inheritParams params
#' @param dataFrame `string`. Data frame class to return. Can be set globally
#'   using the `basejump.data.frame` option.
#'
#' Current recommendations (by priority):
#'
#' - `data.frame`: Base R default. Generally recommended.
#'   - S3 class.
#'   - Allows rownames, but they're required and can't be set `NULL`.
#'   - See `help(topic = "data.frame", package = "base")` for details.
#' - `DataFrame`: Recommended when working with Bioconductor packages.
#'   - S4 class; inherits `DataTable` virtual class.
#'   - Allows rownames, but they're optional and can be set `NULL`.
#'   - See `help(topic = "DataFrame", package = "S4Vectors")` for details.
#' - `tibble`: Recommended when working with tidyverse packages.
#'   - S3 class (`tbl_df`); inherits `data.frame`.
#'   - Does not allow rownames.
#'   - See `help(topic = "tibble", package = "tibble")` for details.
#' - `data.table`: Recommended when working with the data.table package.
#'   - S3 class; inherits `data.frame`.
#'   - Does not allow rownames.
#'   - See `help(topic = "data.table", package = "data.table")` for details.
#'
#' @return Varies, depending on the file extension:
#'
#' - Default: `DataFrame`.
#' - `MTX`: `sparseMatrix`.
#' - `GTF`/`GFF`: `GRanges`.
#' - `JSON`/`YAML`: `list`.
#' - Source code or log: `character`.
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix](https://cran.r-project.org/web/packages/Matrix/index.html).
#'
#' @examples
#' url <- basejumpCacheURL
#'
#' ## R Data
#' x <- import(file = file.path(url, "rnaseq_counts.rda"))
#' class(x)
#'
#' ## Comma Separated Values
#' x <- import(file = file.path(url, "example.csv"))
#' print(x)
#'
#' ## Tab Separated Values
#' x <- import(file = file.path(url, "example.tsv"))
#'
#' ## Microsoft Excel Worksheet
#' x <- import(file = file.path(url, "example.xlsx"))
#' print(x)
#'
#' ## GTF/GFF
#' x <- import(file = file.path(url, "example.gtf"))
#' summary(x)
#'
#' ## JSON
#' x <- import(file = file.path(url, "example.json"))
#' names(x)
#'
#' ## YAML
#' x <- import(file = file.path(url, "example.yml"))
#' names(x)
#'
#' ## Counts Table (i.e. aligned counts from bcbio)
#' x <- import(file = file.path(url, "example.counts"))
#' colSums(x)
import <- function(
    file,
    ...,
    dataFrame = getOption(
        x = "basejump.data.frame",
        default = c("data.frame", "DataFrame", "tibble", "data.table")
    )

) {
    file <- localOrRemoteFile(file)
    dataFrame <- match.arg(dataFrame)
    args <- list(file, ...)

    # Note that matching is case insensitive.
    ext <- basename(file) %>%
        str_match(extPattern) %>%
        .[1L, 2L] %>%
        toupper()

    # Error on blacklisted extension.
    if (ext %in% c("DOC", "DOCX", "PDF", "PPT", "PPTX", "TXT")) {
        stop(paste0(
            "Import of ", basename(file), " failed.\n",
            ext, " extension is not supported."
        ))
    }

    # How we declare NA strings depends on the file extension.
    if (ext %in% c("CSV", "TSV")) {
        message(paste(
            "Importing", basename(file), "using data.table::fread()."
        ))
        args[["na.strings"]] <- naStrings
        data <- do.call(what = fread, args = args)
    } else if (ext %in% c("XLS", "XLSX")) {
        message(paste(
            "Importing", basename(file), "using readxl::read_excel()."
        ))
        args[["na"]] <- naStrings
        data <- do.call(what = read_excel, args = args)
    } else if (ext %in% c("LOG", "MD", "PY", "R", "RMD", "SH")) {
        message(paste(
            "Importing", basename(file), "using readr::read_lines()."
        ))
        data <- do.call(what = read_lines, args = args)
    } else if (ext %in% c("COLNAMES", "ROWNAMES")) {
        message(paste(
            "Importing", basename(file), "using readr::read_lines()."
        ))
        args[["na"]] <- naStrings
        data <- do.call(what = read_lines, args = args)
    } else if (ext == "COUNTS") {
        # bcbio count matrix.
        message(paste(
            "Importing", basename(file), "using readr::read_tsv()."
        ))
        args[["na"]] <- naStrings
        data <- do.call(what = read_tsv, args = args)
        assert_is_subset("id", colnames(data))
        assert_has_no_duplicates(data[["id"]])
        # Coerce to matrix.
        data <- data %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else if (ext %in% c("GFF", "GFF3", "GTF")) {
        message(paste(
            "Importing", basename(file), "using rtracklayer::import()."
        ))
        requireNamespace("rtracklayer")
        data <- tryCatch(
            expr = do.call(what = rtracklayer::import, args = args),
            error = function(e) {
                stop("GFF file failed to load.")  # nocov
            },
            warning = function(w) {
                stop("GFF file failed to load.")  # nocov
            }
        )
    } else if (ext == "MTX") {
        # MatrixMarket sparse matrix.
        # Requires `.rownames` and `.colnames` files.
        message(paste(
            "Importing", basename(file), "using Matrix::readMM()."
        ))
        data <- do.call(what = readMM, args = args)

        # Import the rownames using the sidecar file.
        rownamesFile <- localOrRemoteFile(paste(file, "rownames", sep = "."))
        message(paste(
            "Importing", basename(rownamesFile), "using readr::read_lines()."
        ))
        rownames <- read_lines(file = rownamesFile, na = naStrings)

        # Import the colnames using the sidecar file.
        colnamesFile <- localOrRemoteFile(paste(file, "colnames", sep = "."))
        message(paste(
            "Importing", basename(colnamesFile), "using readr::read_lines()."
        ))
        colnames <- read_lines(file = colnamesFile, na = naStrings)

        # Set the dimnames from the sidecar files.
        rownames(data) <- rownames
        colnames(data) <- colnames
    } else if (ext %in% c("RDA", "RDATA")) {
        message(paste(
            "Importing", basename(file), "using base::load()."
        ))
        safe <- new.env()
        args[["envir"]] <- safe
        object <- do.call(what = load, args = args)
        if (!has_length(safe, n = 1L)) {
            stop("File does not contain a single object.")
        }
        data <- get(object, envir = safe, inherits = FALSE)
    } else if (ext == "RDS") {
        message(paste(
            "Importing", basename(file), "using base::readRDS()."
        ))
        data <- do.call(what = readRDS, args = args)
    } else if (ext == "JSON") {
        message(paste(
            "Importing", basename(file), "using jsonlite::read_json()."
        ))
        data <- do.call(what = read_json, args = args)
    } else if (ext %in% c("YAML", "YML")) {
        message(paste(
            "Importing", basename(file), "using yaml::yaml.load_file()."
        ))
        data <- do.call(what = yaml.load_file, args = args)
    } else {
        message(paste(
            "Importing", basename(file), "using rio::import()."
        ))
        requireNamespace("rio")
        data <- do.call(what = rio::import, args = args)
    }

    if (is.data.frame(data)) {
        # Coerce data frame return, if necessary.
        if (dataFrame == "data.frame") {
            data <- as.data.frame(data)
        } else if (dataFrame == "DataFrame") {
            data <- as(data, "DataFrame")
        } else if (dataFrame == "tibble") {
            data <- as_tibble(data)
        } else if (dataFrame == "data.table") {
            data <- as.data.table(data)
        }

        # Set rownames automatically, if supported.
        if (
            dataFrame %in% c("data.frame", "DataFrame") &&
            "rowname" %in% colnames(data)
        ) {
            message("Setting rownames from `rowname` column.")
            rownames(data) <- data[["rowname"]]
            data[["rowname"]] <- NULL
        }
    }

    data
}
