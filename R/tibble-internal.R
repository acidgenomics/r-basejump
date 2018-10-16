as_tibble <- function(x, ..., rownames = "rowname") {
    tibble::as_tibble(
        x = as.data.frame(x),
        ...,
        rownames = rownames
    )
}
