#' Assign multiple objects to a new environment
#'
#' @rdname assign
#'
#' @param ... List of objects to assign.
#' @param envir_name Environment name.
#'
#' @return Object names.
#'
#' @export
#' @examples
#' assignAsNewEnv(mtcars, starwars, envir_name = "testenv")
assignAsNewEnv <- function(..., envir_name) {
    if (!is_string(envir_name)) {
        stop("Environment name must be a string.")
    }
    envir <- new.env()
    dots <- dots(...)
    objs <- get_objs_from_dots(dots)
    lapply(seq_along(objs), function(a) {
        assign(objs[a], dots[[a]], envir = envir)
    }
    ) %>% invisible
    message(paste("Assigning", toString(objs), "to", envir_name))
    assign(envir_name, envir, parent.frame())
    objs
}

#' @rdname aliases
#' @usage NULL
#' @export
assign_as_new_env <- assignAsNewEnv



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
    string %>%
        as.character %>%
        gsub("^$|^\\s+$|^NA$", NA, .)
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
        as.character %>%
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



#' Remove rows and columns containing only `NA` values
#'
#' @param x Object with column data (e.g. data frame, matrix).
#'
#' @return Sanitized data.
#' @export
#'
#' @examples
#' # Remove NA only rows and columns
#' matrix(c(1, NA, 3,
#'          NA, NA, NA,
#'          2, NA, 4),
#'        nrow = 3, ncol = 3) %>% removeNA
#'
#' data.frame(a = c("A", NA, "C"),
#'            b = c(NA, NA, NA),
#'            c = c("B", NA, "D")) %>% removeNA
#'
#' tibble(a = c("A", NA, "C"),
#'        b = c(NA, NA, NA),
#'        c = c("B", NA, "D")) %>% removeNA
#'
#'
#' # Return unmodified
#' list(a = c("A", NA, "C"),
#'      b = c(NA, NA, NA),
#'      c = c("B", NA, "D")) %>% removeNA
removeNA <- function(x) {
    if (!any(is.data.frame(x), is.matrix(x))) {
        message("Only applicable to column data")
        x
    } else {
        x %>%
            # Remove all NA rows
            .[apply(., 1, function(a) !all(is.na(a))), ] %>%
            # Remove all NA columns
            .[, apply(., 2, function(a) !all(is.na(a)))]
    }
}

#' @rdname aliases
#' @usage NULL
#' @export
remove_na <- removeNA



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
        na.omit %>%
        sort %>%
        unique
}

#' @rdname aliases
#' @usage NULL
#' @export
sort_unique <- sortUnique



#' Update all installed packages
#'
#' Ensure that all GitHub, Bioconductor, and CRAN packages are up to date.
#'
#' @export
update_packages <- function() {
    # Update Bioconductor packages first
    bioc()

    # Now update packages from GitHub repos
    devtools::update_packages()

    # Ensure safe developer environment
    biocValid()
}




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
            fixNA
    }
    mutate_all(df, funs(replace))
}
