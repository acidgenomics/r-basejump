#' Read CSV files inside `data-raw` directory
#'
#' @export
readDataRaw <- function() {
    csv <- list.files("data-raw", pattern = "*.csv", full.names = TRUE)
    lapply(seq_along(csv), function(a) {
        name <- basename(csv[a]) %>% file_path_sans_ext
        df <- read_csv(csv[a])
        dir.create("data", showWarnings = FALSE)
        assign(name, df)
        save(list = name, file = file.path("data", str_c(name, ".rda")))
    }) %>%
        invisible
}



#' Read file by extension
#'
#' Supports automatic loading of standard `.csv`, `.mtx`, `.tsv`, and `.xlsx`
#' files. Also supports bcbio-nextgen pipeline-specific `.counts`, `.colnames`,
#' and `.rownames` files.
#'
#' @param file File path.
#' @param ... Additional parameters.
#'
#' @return [tibble] by default, or a sparse matrix for `.mtx` files.
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix::readMM()]: Read a MatrixMarket file.
readFileByExtension <- function(file, ...) {
    if (is.null(file)) {
        return(NULL)
    }
    if (!is.character(file)) {
        stop("File path must be a string")
    }

    filePath <- normalizePath(file)
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

    # Coerce data frame to tibble, if necessary
    if (is.data.frame(data) & !is_tibble(data)) {
        data <- as(data, "tibble")
    }

    # Return
    if (is_tibble(data)) {
        data %>%
            remove_na %>%
            snake(rownames = FALSE)
    } else {
        data
    }
}



#' Read YAML file
#'
#' @param file YAML file.
#'
#' @return list.
#' @export
readYAML <- function(file) {
    if (file.exists(file)) {
        yaml.load_file(file)
    } else {
        NULL
    }
}
