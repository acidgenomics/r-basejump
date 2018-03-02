.uniqueSymbol <- function(object) {
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }
    assert_is_subset("symbol", colnames(data))
    data[["symbol"]] <- make.unique(data[["symbol"]])
    if (is(object, "GRanges")) {
        mcols(object) <- data
    } else {
        object <- data
    }
    object
}
