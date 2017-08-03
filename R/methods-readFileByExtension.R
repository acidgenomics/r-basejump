#' Read File by Extension
#'
#' Supports automatic loading of standard `.csv`, `.mtx`, `.tsv`, and `.xlsx`
#' files. Also supports bcbio-nextgen pipeline-specific `.counts`, `.colnames`,
#' and `.rownames` files.
#'
#' @rdname readFileByExtension
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
setMethod("readFileByExtension", "character", function(
    object,
    makeNames = "camel",
    ...) {
    filePath <- normalizePath(object)
    fileName <- basename(filePath)
    if (!file.exists(filePath)) {
        stop(paste(fileName, "not found"))
    }

    message(paste("Reading", fileName))

    # Detect file extension
    if (grepl("\\.[a-z]+$", fileName)) {
        ext <- str_match(fileName, "\\.([a-z]+)$")[[2L]]
    } else {
        stop("File extension missing")
    }

    # File import, based on extension
    if (ext == "csv") {
        data <- read_csv(filePath, progress = FALSE, ...)
    } else if (ext == "mtx") {
        data <- readMM(filePath, ...)
    } else if (ext == "tsv") {
        data <- read_tsv(filePath, progress = FALSE, ...)
    } else if (ext == "txt") {
        data <- read_delim(filePath, progress = FALSE, ...)
    } else if (ext == "xlsx") {
        data <- read_excel(filePath, ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- read_lines(filePath, ...)
    } else if (ext == "counts") {
        data <- read_tsv(filePath, progress = FALSE, ...)
    } else {
        stop("Unsupported file type")
    }

    # Coerce data.frame to tibble
    if (is.data.frame(data) & !is_tibble(data)) {
        data <- as(data, "tibble")
    }

    # Sanitize colnames, if desired
    if (!is.null(colnames(data)) &
        makeNames %in% c("camel", "snake")) {
        makeNames <- get(makeNames)
        if (!is.function(makeNames)) stop("makeNames function failure")
        colnames(data) <- makeNames(colnames(data))
    }

    # Remove all NA rows and columns from column data
    if (!is.null(dimnames)) {
        data <- removeNA(data)
    }

    data
})
