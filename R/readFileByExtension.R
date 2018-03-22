#' Read File by Extension
#'
#' Supports automatic loading of standard `.csv`, `.mtx`, `.tsv`, and `.xlsx`
#' files. Also supports bcbio-nextgen pipeline-specific `.counts`, `.colnames`,
#' and `.rownames` files.
#'
#' @family Read Functions
#'
#' @inheritParams general
#' @inheritParams localOrRemoteFile
#' @inheritParams saveData
#' @param object File path.
#' @param makeNames Make syntactically valid names. Supports **`camel`**,
#'   `dotted`, `snake`, or `upperCamel`. This is always enforced.
#'
#' @return `tibble` by default, or a sparse matrix for `.mtx` files.
#' @export
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix::readMM()]: Read a MatrixMarket file.
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
    object,
    makeNames = c("camel", "snake", "upperCamel", "dotted"),
    severity = c("stop", "warning", "message", "none"),
    ...
) {
    assert_is_a_string(object)
    # Require that input string contains an extension
    extPattern <- "\\.([a-zA-Z0-9]+)$"
    assert_all_are_matching_regex(object, extPattern)
    makeNames <- match.arg(makeNames)
    makeNames <- get(
        x = makeNames,
        envir = asNamespace("basejump"),
        inherits = FALSE
    )
    severity <- match.arg(severity)

    file <- localOrRemoteFile(object, severity = severity)
    basename <- names(file)
    ext <- str_match(basename, extPattern)[[2L]]

    # File import, based on extension
    inform(paste("Reading", names(file)))

    na <- c("", "NA", "#N/A")

    # Add extension to tempfile, if necessary
    if (!grepl(extPattern, file)) {
        # tempfile needs an extension or `read_excel()` call will abort
        file.rename(from = file, to = paste0(file, ".", ext))
        file <- paste0(file, ".", ext)
    }

    if (ext == "csv") {
        data <- readr::read_csv(
            file = file,
            na = na,
            progress = FALSE,
            ...
        )
    } else if (ext == "mtx") {
        data <- Matrix::readMM(file = file, ...)
    } else if (ext == "tsv") {
        data <- readr::read_tsv(
            file = file,
            na = na,
            progress = FALSE,
            ...
        )
    } else if (ext == "txt") {
        data <- utils::read.table(
            file = file,
            header = TRUE,
            na.strings = na,
            ...
        )
    } else if (ext == "xlsx") {
        data <- readxl::read_excel(
            path = file,
            na = na,
            ...
        )
    } else if (ext %in% c("colnames", "rownames")) {
        data <- readr::read_lines(
            file = file,
            na = na,
            ...
        )
    } else if (ext == "counts") {
        data <- readr::read_tsv(
            file = file,
            na = na,
            progress = FALSE,
            ...
        ) %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else {
        abort(paste("Unsupported file extension:", basename(file)))
    }

    # Sanitize colnames
    if (!is.null(colnames(data))) {
        colnames(data) <- makeNames(colnames(data))
    }

    # Remove all NA rows and columns from column data
    if (!is.null(dimnames)) {
        data <- removeNA(data)
    }

    data
}
