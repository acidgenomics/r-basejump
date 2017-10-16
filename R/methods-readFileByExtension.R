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
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @param object File path.
#' @param makeNames Make syntactically valid names. Supports **`camel`**,
#'   `snake`, or `FALSE`.
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
#' readFileByExtension(file.path(testDataURL, "mtcars.csv"))
NULL



# Constructors ====
#' @importFrom Matrix readMM
#' @importFrom stringr str_match
#' @importFrom readr read_csv read_lines read_tsv
#' @importFrom readxl read_excel
#' @importFrom tibble column_to_rownames
#' @importFrom utils read.table
.readFileByExtension <- function(
    object,
    makeNames = "camel",
    quiet = FALSE,
    ...) {
    file <- .localOrRemoteFile(object, quiet = quiet)

    # Detect file extension
    extPattern <- "\\.([a-zA-Z0-9]+)$"
    if (!grepl(x = names(file), pattern = extPattern)) {
        stop("File extension missing")
    }
    ext <- str_match(names(file), extPattern) %>%
        .[[2L]]

    # Rename tmpfile to include extension if necessary
    if (!grepl(x = file, pattern = extPattern)) {
        newfile <- paste0(file, ".", ext)
        file.rename(file, newfile)
        file[[1L]] <- newfile
    }

    # File import, based on extension
    if (!isTRUE(quiet)) {
        message(paste("Reading", names(file)))
    }
    if (ext == "csv") {
        data <- readr::read_csv(file, progress = FALSE, ...)
    } else if (ext == "mtx") {
        data <- Matrix::readMM(file, ...)
    } else if (ext == "tsv") {
        data <- readr::read_tsv(file, progress = FALSE, ...)
    } else if (ext == "txt") {
        data <- utils::read.table(file, header = TRUE, ...)
    } else if (ext == "xlsx") {
        data <- readxl::read_excel(file, ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- readr::read_lines(file, ...)
    } else if (ext == "counts") {
        data <- readr::read_tsv(file, progress = FALSE, ...) %>%
            as.data.frame() %>%
            column_to_rownames("id") %>%
            as.matrix()
    } else {
        stop("Unsupported file type")
    }

    # Sanitize colnames, if desired
    if (!is.null(colnames(data)) &
        makeNames %in% c("camel", "snake")) {
        makeNames <- get(makeNames)
        if (!is.function(makeNames)) {
            stop("makeNames function failure")
        }
        colnames(data) <- makeNames(colnames(data))
    }

    # Remove all NA rows and columns from column data
    if (!is.null(dimnames)) {
        data <- removeNA(data)
    }

    data
}



# Methods ====
#' @rdname readFileByExtension
#' @export
setMethod(
    "readFileByExtension",
    signature("character"),
    .readFileByExtension)
