# Extend dplyr method support for S4 DataFrame class.
# These methods should mimic tbl_df support, so documentation isn't necessary.



# Dependencies =================================================================
#' @importFrom dplyr groups
#' @export
dplyr::groups

#' @method groups DataFrame
#' @export
groups.DataFrame <- function(x) {
    # DataFrame is never grouped.
    NULL
}



#' @importFrom dplyr tbl_vars
#' @export
dplyr::tbl_vars

#' @method tbl_vars DataFrame
#' @export
tbl_vars.DataFrame <- function(x) {
    out <- tbl_vars(as(x, "tbl_df"))
    # Always ignore `rowname` column.
    setdiff(out, "rowname")
}



# Single table verbs ===========================================================
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
    .data %>%
        as("tbl_df") %>%
        filter(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr filter_all
#' @export
dplyr::filter_all

#' @method filter_all DataFrame
#' @export
filter_all.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        filter_all(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr filter_at
#' @export
dplyr::filter_at

#' @method filter_at DataFrame
#' @export
filter_at.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        filter_at(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr filter_if
#' @export
dplyr::filter_if

#' @method filter_if DataFrame
#' @export
filter_if.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        filter_if(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr transmute
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



# This requires `groups()` export.

#' @importFrom dplyr mutate_all
#' @export
dplyr::mutate_all

#' @method mutate_all DataFrame
#' @export
mutate_all.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        mutate_all(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr mutate_at
#' @export
dplyr::mutate_at

#' @method mutate_at DataFrame
#' @export
mutate_at.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        mutate_at(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr mutate_if
#' @export
dplyr::mutate_if

#' @method mutate_if DataFrame
#' @export
mutate_if.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        mutate_if(...) %>%
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



#' @importFrom dplyr rename_all
#' @export
dplyr::rename_all

#' @method rename_all DataFrame
#' @export
rename_all.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        rename_all(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr rename_at
#' @export
dplyr::rename_at

#' @method rename_at DataFrame
#' @export
rename_at.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        rename_at(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr rename_if
#' @export
dplyr::rename_if

#' @method rename_if DataFrame
#' @export
rename_if.DataFrame <- function(.data, ...) {
    .data %>%
        as("tbl_df") %>%
        rename_if(...) %>%
        as("DataFrame")
}



#' @importFrom dplyr select
#' @export
dplyr::select

#' @method select DataFrame
#' @export
select.DataFrame <- function(.data, ...) {
    .data %>%
        as_tibble(rownames = NULL) %>%
        select(...) %>%
        as("DataFrame") %>%
        set_rownames(rownames(.data))
}



#' @importFrom dplyr select_at
#' @export
dplyr::select_at

#' @method select_at DataFrame
#' @export
select_at.DataFrame <- function(.data, ...) {
    .data %>%
        as_tibble(rownames = NULL) %>%
        select_at(...) %>%
        as("DataFrame") %>%
        set_rownames(rownames(.data))
}



#' @importFrom dplyr select_if
#' @export
dplyr::select_if

#' @method select_if DataFrame
#' @export
select_if.DataFrame <- function(.data, ...) {
    .data %>%
        as_tibble(rownames = NULL) %>%
        select_if(...) %>%
        as("DataFrame") %>%
        set_rownames(rownames(.data))
}



#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @method transmute DataFrame
#' @export
transmute.DataFrame <- function(.data, ...) {
    .data %>%
        as_tibble(rownames = NULL) %>%
        transmute(...) %>%
        as("DataFrame") %>%
        set_rownames(rownames(.data))
}



#' @importFrom dplyr transmute_at
#' @export
dplyr::transmute_at

#' @method transmute_at DataFrame
#' @export
transmute_at.DataFrame <- function(.data, ...) {
    .data %>%
        as_tibble(rownames = NULL) %>%
        transmute_at(...) %>%
        as("DataFrame") %>%
        set_rownames(rownames(.data))
}



#' @importFrom dplyr transmute_if
#' @export
dplyr::transmute_if

#' @method transmute_if DataFrame
#' @export
transmute_if.DataFrame <- function(.data, ...) {
    .data %>%
        as_tibble(rownames = NULL) %>%
        transmute_if(...) %>%
        as("DataFrame") %>%
        set_rownames(rownames(.data))
}



# Two table verbs ==============================================================
#' @importFrom dplyr left_join
#' @export
dplyr::left_join

#' @method left_join DataFrame
#' @export
left_join.DataFrame <- function(x, y, ...) {
    data <- left_join(
        x = as_tibble(x, rownames = "rowname"),
        y = as_tibble(y, rownames = NULL),
        ...
    )
    as(data, "DataFrame")
}
