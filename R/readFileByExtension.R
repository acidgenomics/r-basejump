#' Read File by Extension
#'
#' Supports automatic loading of common file extensions:
#'
#' - `csv`: Comma Separated Values.
#' - `gff`/`gff3`/`gtf`: General Feature Format.
#' - `mtx`: MatrixMarket sparse matrix.
#' - `rda`/`RData`: R Data. Must contain a single object. Doesn't require
#'   internal object name to match, like in the [loadData()] function.
#' - `rds`: R Data Serialized.
#' - `tsv` Tab Separated Values.
#' - `xlsx`: Excel workbook.
#' - `yaml`/`yml`: YAML.
#'
#' Also supports some additional extensions commonly used with the
#' [bcbio](https://bcbio-nextgen.readthedocs.io) pipeline:
#'
#' - `counts`: Counts `table`.
#' - `colnames`: Sidecar file containing column names.
#' - `rownames`: Sidecar file containing row names.
#'
#' If the file format isn't supported natively (or blacklisted), the
#' [rio](https://cran.r-project.org/web/packages/rio/index.html) package will
#' be used as a fallback attempt.
#'
#' @note Reading a MatrixMarket ("`mtx`") file now requires "`colnames`" and
#'   `"rownames"` sidecar files containing the [colnames()] and [rownames()] of
#'   the sparse matrix. Legacy support for manual loading of these sidecar files
#'   is provided.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `tbl_df` by default, or a sparse matrix for `.mtx` files.
#' @export
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix](https://cran.r-project.org/web/packages/Matrix/index.html).
#'
#' @examples
#' # Comma Separated Values
#' x <- readFileByExtension("http://basejump.seq.cloud/mtcars.csv")
#' glimpse(x)
#'
#' # Counts Table (bcbio)
#' x <- readFileByExtension("http://basejump.seq.cloud/example.counts")
#' glimpse(x)
#'
#' # R Data
#' x <- readFileByExtension("http://basejump.seq.cloud/rnaseqCounts.rda")
#' glimpse(x)
#'
#' # Microsoft Excel Worksheet
#' x <- readFileByExtension("http://basejump.seq.cloud/mtcars.xlsx")
#' glimpse(x)
#'
#' # Text Table (ambiguous; not recommended)
#' x <- suppressWarnings(
#'     readFileByExtension("http://basejump.seq.cloud/example.txt")
#' )
#' glimpse(x)
readFileByExtension <- function(file, ...) {
    assert_is_a_string(file)
    file <- localOrRemoteFile(file)

    ext <- str_match(basename(file), extPattern)[1L, 2L]
    exti <- tolower(ext)  # case insensitive
    unsupported <- paste("Unsupported extension", ":", ext)

    # File import, based on extension
    message(paste("Reading", basename(file)))

    # Sanitize NA values
    na <- c("", "NA", "#N/A", "NULL", "null")

    blacklist <- c(
        "doc",
        "docx",
        "ppt",
        "pptx"
    )
    source <- c(
        "md",
        "py",
        "r",
        "rmd",
        "sh"
    )
    if (exti %in% blacklist) {
        stop(unsupported)  # nocov
    } else if (exti %in% source) {
        message("Importing as source code lines")
        data <- read_lines(file)
    } else if (exti %in% c("colnames", "rownames")) {
        data <- read_lines(file = file, na = na, ...)
    } else if (exti == "counts") {
        # bcbio counts output
        data <- read_tsv(
            file = file,
            na = na,
            progress = FALSE,
            ...
        ) %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else if (exti == "csv") {
        # Comma separated values
        data <- readr::read_csv(
            file = file,
            na = na,
            progress = FALSE,
            ...
        )
    } else if (exti %in% c("gff", "gff3", "gtf")) {
        data <- suppressMessages(readGFF(file))
    } else if (exti == "mtx") {
        # MatrixMarket
        # Require `.rownames` and `.colnames` files
        data <- readMM(file = file, ...)
        rownames <- localOrRemoteFile(paste(file, "rownames", sep = ".")) %>%
            read_lines(na = na)
        colnames <- localOrRemoteFile(paste(file, "colnames", sep = ".")) %>%
            read_lines(na = na)
        rownames(data) <- rownames
        colnames(data) <- colnames
    } else if (exti %in% c("rda", "rdata")) {
        safe <- new.env()
        object <- load(file, envir = safe)
        if (length(safe) != 1L) {
            stop("File does not contain a single object")
        }
        data <- get(object, envir = safe, inherits = FALSE)
    } else if (exti == "rds") {
        data <- readRDS(file)
    } else if (exti == "tsv") {
        # Tab separated values
        data <- read_tsv(file = file, na = na, progress = FALSE, ...)
    } else if (exti == "txt") {
        # Table?
        warning(paste(
            paste(deparse(ext), "is ambiguous and not recommended."),
            "Assuming table output from `utils::write.table()`.",
            sep = "\n"
        ))
        data <- read.table(
            file = file,
            header = TRUE,
            na.strings = na,
            ...
        )
    } else if (exti == "xlsx") {
        # Excel workbook
        data <- read_excel(path = file, na = na, ...)
    } else if (exti %in% c("yaml", "yml")) {
        data <- suppressMessages(readYAML(file))
    } else if (requireNamespace("rio", quietly = TRUE)) {
        # nocov start
        message(paste(
            paste(deparse(ext), "isn't natively supported."),
            "Attempting to read using `rio::import()`.",
            sep = "\n"
        ))
        data <- rio::import(file)
        # nocov end
    } else {
        # nocov start
        stop(paste(
            unsupported,
            "Install the rio package for additional file format support.",
            sep = "\n"
        ))
        # nocov end
    }

    data
}
