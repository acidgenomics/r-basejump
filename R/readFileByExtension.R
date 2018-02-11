#' Read File by Extension
#'
#' Supports automatic loading of standard `.csv`, `.mtx`, `.tsv`, and `.xlsx`
#' files. Also supports bcbio-nextgen pipeline-specific `.counts`, `.colnames`,
#' and `.rownames` files.
#'
#' @rdname readFileByExtension
#' @name readFileByExtension
#' @family Data Import and Project Utilities
#'
#' @importFrom Matrix readMM
#' @importFrom stringr str_match
#' @importFrom readr read_csv read_lines read_tsv
#' @importFrom readxl read_excel
#' @importFrom tibble column_to_rownames
#' @importFrom utils read.table
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param object File path.
#' @param makeNames Make syntactically valid names. Supports **`camel`**,
#'   `dotted`, `snake`, or `upperCamel`. This is always enforced.
#'
#' @return [tibble] by default, or a sparse matrix for `.mtx` files.
#' @export
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix::readMM()]: Read a MatrixMarket file.
#'
#' @examples
#' readFileByExtension("http://basejump.seq.cloud/mtcars.csv")
readFileByExtension <- function(
    object,
    makeNames = "camel",
    quiet = FALSE,
    ...) {
    assert_is_a_string(object)
    # Require that input string contains an extension
    extPattern <- "\\.([a-zA-Z0-9]+)$"
    assert_all_are_matching_regex(object, extPattern)
    assert_is_a_string(makeNames)
    assert_is_subset(makeNames, c("camel", "dotted", "snake" , "upperCamel"))
    assert_is_a_bool(quiet)

    file <- localOrRemoteFile(object, quiet = quiet)
    basename <- names(file)
    ext <- str_match(basename, extPattern)[[2L]]

    # File import, based on extension
    if (!isTRUE(quiet)) {
        inform(paste("Reading", names(file)))
    }

    na <- c("", "NA", "#N/A")

    if (ext == "csv") {
        data <- readr::read_csv(
            file = file,
            na = na,
            progress = FALSE,
            ...)
    } else if (ext == "mtx") {
        data <- Matrix::readMM(file = file, ...)
    } else if (ext == "tsv") {
        data <- readr::read_tsv(
            file = file,
            na = na,
            progress = FALSE,
            ...)
    } else if (ext == "txt") {
        data <- utils::read.table(
            file = file,
            header = TRUE,
            na.strings = na,
            ...)
    } else if (ext == "xlsx") {
        data <- readxl::read_excel(
            path = file,
            na = na,
            ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- readr::read_lines(
            file = file,
            na = na,
            ...)
    } else if (ext == "counts") {
        data <- readr::read_tsv(
            file = file,
            na = na,
            progress = FALSE,
            ...) %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else {
        abort("Unsupported file type")
    }

    # Sanitize colnames
    if (!is.null(colnames(data))) {
        makeNames <- get(makeNames)
        colnames(data) <- makeNames(colnames(data))
    }

    # Remove all NA rows and columns from column data
    if (!is.null(dimnames)) {
        data <- removeNA(data)
    }

    data
}
