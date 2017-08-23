#' Read File by Extension
#'
#' Supports automatic loading of standard `.csv`, `.mtx`, `.tsv`, and `.xlsx`
#' files. Also supports bcbio-nextgen pipeline-specific `.counts`, `.colnames`,
#' and `.rownames` files.
#'
#' @rdname readFileByExtension
#' @name readFileByExtension
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



# Methods ====
#' @rdname readFileByExtension
#' @export
setMethod("readFileByExtension", "character", function(
    object,
    makeNames = "camel",
    ...) {
    file <- .localOrRemoteFile(object)
    message(paste("Reading", names(file)))

    # Detect file extension
    extPattern <- "\\.([a-zA-Z0-9]+)$"
    if (!grepl(extPattern, names(file))) {
        stop("File extension missing")
    }
    ext <- str_match(names(file), extPattern) %>%
            .[[2L]]

    # File import, based on extension
    if (ext == "csv") {
        data <- read_csv(file, progress = FALSE, ...)
    } else if (ext == "mtx") {
        data <- readMM(file, ...)
    } else if (ext == "tsv") {
        data <- read_tsv(file, progress = FALSE, ...)
    } else if (ext == "txt") {
        data <- read_delim(file, progress = FALSE, ...)
    } else if (ext == "xlsx") {
        data <- read_excel(file, ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- read_lines(file, ...)
    } else if (ext == "counts") {
        data <- read_tsv(file, progress = FALSE, ...)
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
