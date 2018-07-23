#' Read File by Extension
#'
#' Supports automatic loading of common file extensions:
#'
#' - `csv`: Comma Separated Values.
#' - `gff`/`gff3`/`gtf`: General Feature Format.
#' - `mtx`: MatrixMarket sparse matrix.
#' - `rda`/`RData`: R Data. Must contain a single object. Doesn't require
#'   internal object name to match, like with the [loadData()] function.
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
#' @return Varies depending on the file extension.
#' @export
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix](https://cran.r-project.org/web/packages/Matrix/index.html).
#'
#' @examples
#' # R Data
#' x <- readFileByExtension("http://basejump.seq.cloud/rnaseq_counts.rda")
#' glimpse(x)
#'
#' # Comma Separated Values
#' x <- readFileByExtension("http://basejump.seq.cloud/mtcars.csv")
#' glimpse(x)
#'
#' # Microsoft Excel Worksheet
#' x <- readFileByExtension("http://basejump.seq.cloud/mtcars.xlsx")
#' glimpse(x)
#'
#' # bcbio Counts Table
#' x <- readFileByExtension("http://basejump.seq.cloud/example.counts")
#' glimpse(x)
readFileByExtension <- function(file, ...) {
    assert_is_a_string(file)
    file <- localOrRemoteFile(file)
    message(paste("Reading", basename(file)))

    ext <- basename(file) %>%
        str_match(extPattern) %>%
        .[1L, 2L] %>%
        tolower()

    na <- c("", "NA", "#N/A", "NULL", "null")
    blacklist <- c("doc", "docx", "ppt", "pptx", "txt")
    source <- c("md", "py", "r", "rmd", "sh")
    unsupported <- paste("Unsupported extension", ":", deparse(ext))

    if (ext %in% blacklist) {
        stop(unsupported)  # nocov
    } else if (ext %in% source) {
        message("Importing as source code lines")
        data <- read_lines(file, ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- read_lines(file = file, na = na, ...)
    } else if (ext == "counts") {
        # bcbio counts output
        data <- read_tsv(file = file, na = na, ...) %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else if (ext %in% c("gff", "gff3", "gtf")) {
        data <- suppressMessages(readGFF(file))
    } else if (ext == "mtx") {
        # MatrixMarket
        # Require `.rownames` and `.colnames` files
        data <- readMM(file = file, ...)
        rownames <- localOrRemoteFile(paste(file, "rownames", sep = ".")) %>%
            read_lines(na = na)
        colnames <- localOrRemoteFile(paste(file, "colnames", sep = ".")) %>%
            read_lines(na = na)
        rownames(data) <- rownames
        colnames(data) <- colnames
    } else if (ext %in% c("rda", "rdata")) {
        safe <- new.env()
        object <- load(file, envir = safe)
        if (length(safe) != 1L) {
            stop("File does not contain a single object.")
        }
        data <- get(object, envir = safe, inherits = FALSE)
    } else if (ext == "rds") {
        data <- readRDS(file)
    } else if (ext %in% c("yaml", "yml")) {
        data <- suppressMessages(readYAML(file))
    } else {
        data <- rio::import(file, ...)
    }

    data
}
