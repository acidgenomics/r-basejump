#' Wash Column Data
#'
#' This function cleans poorly formed separators, leading and trailing commas or
#' spaces, empty cells, and sets `NA` values if necessary.
#'
#' @rdname wash
#'
#' @return Clean object.



.wash <- function(object) {
    replace <- function(a) {
        a %>%
            # Duplicate separators
            str_replace_all("(,|;|/)\\s(,|;|/)", "\\1") %>%
            # Leading separators
            str_replace_all("^(,|;|/)\\s", "") %>%
            # Trailing separators
            str_replace_all("\\s(,|;|/)$", "") %>%
            # NAs in string
            str_replace_all("NA,\\s|,\\sNA", "") %>%
            fixNA
    }
    object %>%
        as("tibble") %>%
        mutate_all(funs(replace))
}



#' @rdname wash
#' @export
setMethod("wash", "data.frame", function(object) {
    df <- object %>%
        .wash %>%
        as.data.frame
    if ("rowname" %in% colnames(df)) {
        df <- column_to_rownames(df)
    }
    df
})



#' @rdname wash
#' @export
setMethod("wash", "DataFrame", function(object) {
    df <- object %>%
        .wash %>%
        as("DataFrame")
    if ("rowname" %in% colnames(df)) {
        rownames(df) <- df[["rowname"]]
        df[["rowname"]] <- NULL
    }
    df
})



#' @rdname wash
#' @export
setMethod("wash", "tbl_df", .wash)
