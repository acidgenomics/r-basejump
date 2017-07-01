#' Read CSV files inside `data-raw` directory.
#'
#' @export
readDataRaw <- function() {
    csv <- list.files("data-raw", pattern = "*.csv", full.names = TRUE)
    lapply(seq_along(csv), function(a) {
        name <- basename(csv[a]) %>% file_path_sans_ext
        df <- read_csv(csv[a])
        dir.create("data", showWarnings = FALSE)
        assign(name, df)
        save(list = name, file = file.path("data", paste0(name, ".rda")))
    }
    ) %>% invisible
}

#' @rdname readDataRaw
#' @usage NULL
#' @export
readDataRaw -> read_data_raw  # nolint



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
#' - [readMM()]: Read a MatrixMarket file.
readFileByExtension <- function(file, ...) {
    if (is.null(file)) {
        return(NULL)
    }
    if (!is.character(file)) {
        stop("File path must be a string")
    }

    file_path <- normalizePath(file)
    file_name <- basename(file_path)

    if (!file.exists(file_path)) {
        stop(paste(file_name, "not found"))
    }

    message(paste("Reading", file_name))

    # Detect file extension
    if (grepl("\\.[a-z]+$", file_name)) {
        ext <- str_match(file_name, "\\.([a-z]+)$")[[2L]]
    } else {
        stop("File extension missing")
    }

    # File import, based on extension
    if (ext == "csv") {
        data <- read_csv(file_path, progress = FALSE, ...)
    } else if (ext == "mtx") {
        data <- readMM(file_path, ...)
    } else if (ext == "tsv") {
        data <- read_tsv(file_path, progress = FALSE, ...)
    } else if (ext == "txt") {
        data <- read_delim(file_path, progress = FALSE, ...)
    } else if (ext == "xlsx") {
        data <- read_excel(file_path, ...)
    } else if (ext %in% c("colnames", "rownames")) {
        data <- read_lines(file_path, ...)
    } else if (ext == "counts") {
        data <- read_tsv(file_path, progress = FALSE, ...)
    } else {
        stop("Unsupported file type")
    }

    # Coerce data frame to tibble, if necessary
    if (is.data.frame(data) & !is_tibble(data)) {
        data <- as_tibble(data)
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

#' @rdname readFileByExtension
#' @usage NULL
#' @export
readFileByExtension -> read_file_by_extension  # nolint



#' Read YAML file into a list
#'
#' @param file YAML file.
#'
#' @export
readYAML <- function(file) {
    if (file.exists(file)) {
        yaml.load_file(file)
    } else {
        NULL
    }
}

#' @rdname readYAML
#' @usage NULL
#' @export
readYAML -> read_yaml  # nolint
