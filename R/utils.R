#' Assign multiple variables
#'
#' Optionally, you can specify a name prefix and the desired environment.
#'
#' @rdname assign
#'
#' @param ... List of variables to assign.
#' @param envir Desired environment (optional).
#' @param prefix Name prefix (optional).
#'
#' @export
assignMultiple <- function(
    ...,
    envir = parent.frame(),
    prefix = NULL) {
    # `-1` here removes the function name
    names <- sapply(match.call(expand.dots = TRUE)[-1], deparse)
    data <- list(...)
    invisible(lapply(seq_along(data), function(x) {
        name <- names[x]
        if (!is.null(prefix)) {
            name <- paste(c(prefix, name), collapse = "_")
        }
        assign(name, data[[x]], envir = get(envir))
    }))
}

#' @rdname aliases
#' @usage NULL
#' @export
assign_multiple <- assignMultiple



#' Fix empty and `NA` character strings
#'
#' @param string String missing `NA`.
#'
#' @return String containing `NA`.
#' @export
#'
#' @examples
#' fixNA(c(1, "x", "", "NA"))
fixNA <- function(string) {
    gsub("^$|^NA$", NA, string)
}

#' @rdname aliases
#' @usage NULL
#' @export
fix_na <- fixNA



#' [grep()] pattern string generator
#'
#' Generate a [grep()] string for pattern matching against [toString()] return
#' (comma separated).
#'
#' @param identifier Identifier.
#'
#' @return Comma separated string for matching against [toString()] return.
#' @export
#'
#' @examples
#' grepString("gene")
grepString <- function(identifier) {
    identifier %>%
        paste0(
            # Unique:
            "^", ., "$",
            "|",
            # Beginning of list:
            "^", ., ",",
            "|",
            # Middle of list:
            "\\s", ., ",",
            "|",
            # End of list:
            "\\s", ., "$")
}

#' @rdname aliases
#' @usage NULL
#' @export
grep_string <- grepString



#' Quickly perform sort unique on a vector
#'
#' The function also strips `NA` values. This is useful for gene list server
#' queries, for example.
#'
#' @param vector Vector with duplicates, `NA` values.
#'
#' @return Unique vector.
#' @export
#'
#' @examples
#' sortUnique(c("milk", "eggs", "eggs", NA))
sortUnique <- function(vector) {
    vector %>%
        stats::na.omit(.) %>%
        sort %>%
        unique
}

#' @rdname aliases
#' @usage NULL
#' @export
sort_unique <- sortUnique




#' Wash a data frame
#'
#' This function cleans poorly formed separators, leading and trailing commas or
#' spaces, empty cells, and sets `NA` values if necessary.
#'
#' @param df Dirty data frame.
#'
#' @return Clean data frame.
#' @export
wash <- function(df) {
    if (!is.data.frame(df)) {
        stop("Object must be a data frame")
    }
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
            # Character NAs
            gsub("^$|^\\s+$|^NA$", NA, .)
    }
    mutate_all(df, funs(replace))
}
