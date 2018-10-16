as_tibble <- function(x, rownames = "rowname") {
    data <- as.data.frame(x)
    if (!hasRownames(data)) {
        rownames <- NULL
    }
    tibble::as_tibble(data, rownames = rownames)
}
