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
#' @note Reading a MatrixMarket ("`mtx`") file now requires "`colnames`" and
#'   `"rownames"` sidecar files containing the [colnames()] and [rownames()] of
#'   the sparse matrix. Legacy support for manual loading of these sidecar files
#'   is provided.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param makeNames Make syntactically valid names. Supports **`camel`**,
#'   `dotted`, `snake`, or `upperCamel`. This is always enforced.
#'
#' @return `tbl_df` by default, or a sparse matrix for `.mtx` files.
#' @export
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix](https://cran.r-project.org/web/packages/Matrix/index.html).
#'
#' Integration of the
#' [rio](https://cran.r-project.org/web/packages/rio/index.html) package is
#' being considered for a future update.
#'
#' @examples
#' # Comma separated values
#' x <- readFileByExtension("http://basejump.seq.cloud/mtcars.csv")
#' glimpse(x)
#'
#' # Microsoft Excel Worksheet
#' x <- readFileByExtension("http://basejump.seq.cloud/mtcars.xlsx")
#' glimpse(x)
#'
#' # R Data
#' x <- readFileByExtension("http://basejump.seq.cloud/rnaseqCounts.rda")
#' glimpse(x)
readFileByExtension <- function(
    file,
    makeNames = c("camel", "dotted", "snake", "upperCamel"),
    ...
) {
    assert_is_a_string(file)
    file <- localOrRemoteFile(file)

    # Get makeNames function
    makeNames <- match.arg(makeNames)
    makeNames <- get(
        x = makeNames,
        envir = asNamespace("basejump"),
        inherits = FALSE
    )

    basename <- names(file)
    ext <- str_match(basename, extPattern)[1L, 2L]
    exti <- tolower(ext)  # case insensitive
    unsupported <-

    # File import, based on extension
    message(paste("Reading", names(file)))

    # Sanitize NA values
    na <- c("", "NA", "#N/A", "NULL", "null")

    blacklist <- c(
        "doc",
        "docx",
        "ppt",
        "pptx",
        "txt"
    )
    source <- c(
        "md",
        "py",
        "r",
        "rmd",
        "sh"
    )
    if (exti %in% blacklist) {
        stop(paste("Unsupported extension", ":", ext))
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
            stop("File does not contain a single object.")
        }
        data <- get(object, envir = safe, inherits = FALSE)
    } else if (exti == "rds") {
        data <- readRDS(file)
    } else if (exti == "tsv") {
        # Tab separated values
        data <- read_tsv(file = file, na = na, progress = FALSE, ...)
    } else if (exti == "xlsx") {
        # Excel workbook
        data <- read_excel(path = file, na = na, ...)
    } else if (exti %in% c("yaml", "yml")) {
        data <- suppressMessages(readYAML(file))
    } else if (requireNamespace("rio", quietly = TRUE)) {
        message(paste(
            paste(deparse(ext), "isn't natively supported."),
            "Attempting to read using `rio::import()`.",
            sep = "\n"
        ))
        data <- rio::import(file)
    } else {
        stop(paste(
            unsupportedExt,
            "Install the rio package for additional file format support.",
            sep = "\n"
        ))
    }

    # Sanitize colnames
    if (!is.null(colnames(data))) {
        colnames(data) <- makeNames(colnames(data))
    }

    # Remove any rows and columns containing only NA values
    if (!is.null(dimnames)) {
        data <- removeNA(data)
    }

    data
}
