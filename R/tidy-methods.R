# bind_cols
# bind_rows
# slice

# Single table verbs ===========================================================
# arrange_all
# arrange_at
# arrange_if
# distinct
# do
# filter
# filter_all
# filter_at
# filter_if
# group_by
# mutate
# mutate_all
# mutate_at
# mutate_if
# rename
# rename_all
# mutate_at
# mutate_if
# select
# select_all
# select_at
# select_if
# summarise
# summarise_all
# summarise_at
# summarise_if
# summarize variants
# transmute
# transmute_all
# transmute_at
# trasmute_if



#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @method arrange DataFrame
#' @export
arrange.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        arrange(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @method filter DataFrame
#' @export
filter.DataFrame <- function(.data, ...) {
    ..data %>%
        as("tbl_df") %>%
        filter(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @method mutate DataFrame
#' @export
mutate.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        mutate(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr rename
#' @export
dplyr::rename

#' @method rename DataFrame
#' @export
rename.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        rename(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr select
#' @export
dplyr::select

#' @method select DataFrame
#' @export
select.DataFrame <- function(.data, ...) {
    rownames <- rownames(.data)
    data <- .data %>%
        as("tbl_df") %>%
        select(...) %>%
        as("DataFrame")
    rownames(data) <- rownames
    data
}



# Two table verbs ==============================================================
#' @importFrom dplyr left_join
#' @export
dplyr::left_join

#' @method left_join DataFrame
#' @export
left_join.DataFrame <- function(x, y, ...) {
    assert_is_all_of(x, "DataFrame")
    assert_is_all_of(y, "DataFrame")
    data <- left_join(
        x = as_tibble(x, rownames = "rowname"),
        y = as_tibble(y, rownames = NULL),
        ...
    )
    as(data, "DataFrame")
}
