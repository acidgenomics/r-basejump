.coerceToSummarizedExperiment <- function(object) {
    assert_is_all_of(object, "SummarizedExperiment")
    if (is(object, "RangedSummarizedExperiment")) {
        object <- as(object, "RangedSummarizedExperiment")
    }
    as(object, "SummarizedExperiment")
}
