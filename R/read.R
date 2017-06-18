#' Read utilities
#'
#' @rdname read



#' @rdname read
#' @description Read CSV files inside `data-raw` directory.
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

#' @rdname snake_aliases
#' @usage NULL
#' @export
read_data_raw <- readDataRaw  # nolint



#' @rdname read
#' @description Read YAML file into a list.
#' @param file YAML file.
#' @export
readYAML <- function(file) {
    if (file.exists(file)) {
        yaml.load_file(file)
    } else {
        NULL
    }
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
read_yaml <- readYAML  # nolint
