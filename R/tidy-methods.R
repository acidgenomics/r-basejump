# arrange ======================================================================
#' @importFrom dplyr arrange
#' @export
dplyr::arrange



#' @method arrange DataFrame
#' @export
arrange.DataFrame <- function(.data, ...) {
    data <- as(.data, "tbl_df")
    data <- filter(data, ...)
    as(data, "DataFrame")
}



# filter =======================================================================
#' @importFrom dplyr filter
#' @export
dplyr::filter



#' @method filter DataFrame
#' @export
filter.DataFrame <- function(.data, ...) {
    data <- as(.data, "tbl_df")
    data <- filter(data, ...)
    as(data, "DataFrame")
}



# left_join ====================================================================
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



# mutate =======================================================================
#' @importFrom dplyr mutate
#' @export
dplyr::mutate



#' @method mutate DataFrame
#' @export
mutate.DataFrame <- function(.data, ...) {
    data <- as(.data, "tbl_df")
    data <- mutate(data, ...)
    as(data, "DataFrame")
}



# rename =======================================================================
#' @importFrom dplyr rename
#' @export
dplyr::rename



#' @method rename DataFrame
#' @export
rename.DataFrame <- function(.data, ...) {
    data <- as(.data, "tbl_df")
    data <- rename(data, ...)
    as(data, "DataFrame")
}



# select =======================================================================
#' @importFrom dplyr select
#' @export
dplyr::select



#' @method select DataFrame
#' @export
select.DataFrame <- function(.data, ...) {
    rownames <- rownames(.data)
    data <- as(.data, "tbl_df")
    data <- select(data, ...)
    data <- as(data, "DataFrame")
    rownames(data) <- rownames
    data
}
