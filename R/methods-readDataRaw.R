#' Read CSV Files Inside `data-raw/` Directory
#'
#' This function automatically loads raw CSVs using the readr package and
#' saves the data as R binary data into the `data/` directory.
#'
#' @rdname readDataRaw
#' @name readDataRaw
#'
#' @family Data Import and Project Utilities
#'
#' @return No value.
NULL



# Methods ====
#' @rdname readDataRaw
#' @export
setMethod("readDataRaw", "missing", function() {
    csv <- list.files("data-raw", pattern = "*.csv", full.names = TRUE)
    lapply(seq_along(csv), function(a) {
        name <- basename(csv[a]) %>% file_path_sans_ext
        df <- read_csv(csv[a])
        dir.create("data", showWarnings = FALSE)
        assign(name, df)
        save(list = name, file = file.path("data", paste0(name, ".rda")))
    }) %>%
        invisible
})
