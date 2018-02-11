.gffKeyValuePairs <- function(object) {
    assert_is_data.frame(object)
    assert_is_subset("keyValuePairs", colnames(object))
    object[["keyValuePairs"]] %>%
        as.character() %>%
        unique()
}
