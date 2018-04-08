#' Read File by Extension
#'
#' Supports automatic loading of standard `.csv`, `.mtx`, `.tsv`, and `.xlsx`
#' files. Also supports bcbio-nextgen pipeline-specific `.counts`, `.colnames`,
#' and `.rownames` files.
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
#' @examples
#' # Comma separated values
#' csv <- readFileByExtension("http://basejump.seq.cloud/mtcars.csv")
#' glimpse(csv)
#'
#' # Microsoft Excel Worksheet
#' xlsx <- readFileByExtension("http://basejump.seq.cloud/mtcars.xlsx")
#' glimpse(xlsx)
readFileByExtension <- function(
    file,
    makeNames = c("camel", "snake", "upperCamel", "dotted"),
    ...
) {
    assert_is_a_string(file)
    file <- localOrRemoteFile(file)
    # Require that input string contains an extension
    extPattern <- "\\.([a-zA-Z0-9]+)(\\.gz)?$"
    assert_all_are_matching_regex(file, extPattern)
    makeNames <- match.arg(makeNames)
    makeNames <- get(
        x = makeNames,
        envir = asNamespace("basejump"),
        inherits = FALSE
    )

    basename <- names(file)
    match <- str_match(basename, extPattern)
    ext <- match[1L, 2L]
    extFull <- paste(match[1L, 2L:3L], collapse = "")
    gzip <- ifelse(is.na(match[1L, 3L]), FALSE, TRUE)

    # File import, based on extension
    message(paste("Reading", names(file)))

    # Sanitize NA values
    na <- c("", "NA", "#N/A", "NULL", "null")

    # Add extension to tempfile, if necessary
    if (!grepl(extPattern, file)) {
        file.rename(from = file, to = paste0(file, ".", extFull))
        file <- paste0(file, ".", extFull)
    }

    if (ext == "csv") {
        # Comma separated values
        data <- readr::read_csv(
            file = file,
            na = na,
            progress = FALSE,
            ...
        )
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
    } else if (ext == "tsv") {
        # Tab separated values
        data <- read_tsv(file = file, na = na, progress = FALSE, ...)
    } else if (ext == "txt") {
        # Text table
        data <- read.table(file = file, header = TRUE, na.strings = na, ...)
    } else if (ext == "xlsx") {
        # Excel workbook
        data <- read_excel(path = file, na = na, ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- read_lines(file = file, na = na, ...)
    } else if (ext == "counts") {
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
    } else {
        stop(paste("Unsupported file extension:", basename(file)))
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
