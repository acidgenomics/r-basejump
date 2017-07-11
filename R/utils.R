#' Assign multiple objects to a new environment
#'
#' @param ... Dot objects to assign.
#' @param envirName Environment name.
#'
#' @return Object names.
#'
#' @export
#' @examples
#' assignAsNewEnv(mtcars, starwars, envirName = "testenv")
assignAsNewEnv <- function(..., envirName) {
    if (!is_string(envirName)) {
        stop("Environment name must be a string")
    }
    envir <- new.env()
    dots <- dots(...)
    objs <- get_objs_from_dots(dots)
    lapply(seq_along(objs), function(a) {
        assign(objs[a], dots[[a]], envir = envir)
    }
    ) %>% invisible
    message(paste("Assigning", toString(objs), "to", envirName))
    assign(envirName, envir, parent.frame())
    objs
}



#' Clear warnings
#'
#' @keywords internal
#'
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}



#' Fix character strings missing `NA`
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
        # Note that [str_replace()] doesn't work with `NA` values
        gsub("^$|^\\s+$|^NA$", NA, .)
}



#' grep string generator
#'
#' Generate a grep string for pattern matching against comma separated
#' [base::toString()] output.
#'
#' @param identifier Identifier.
#'
#' @return Comma separated grep string.
#' @export
#'
#' @examples
#' grepString("gene")
grepString <- function(identifier) {
    identifier %>%
        as.character %>%
        str_c(
            # Unique
            "^", ., "$",
            "|",
            # Beginning of list
            "^", ., ",",
            "|",
            # Middle of list
            "\\s", ., ",",
            "|",
            # End of list
            "\\s", ., "$")
}



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
            .[apply(., 1L, function(a) !all(is.na(a))), ] %>%
            # Remove all NA columns
            .[, apply(., 2L, function(a) !all(is.na(a)))]
    }
}



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



#' Update all installed packages
#'
#' Ensure that all GitHub, Bioconductor, and CRAN packages are up to date.
#'
#' Internaly this is a wrapper for [devtools::update_packages()] that improves
#' support for Bioconductor.
#'
#' @keywords internal
#'
#' @export
updatePackages <- function() {
    # Update Bioconductor packages first
    biocLite()

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
#' @param x Dirty data frame.
#'
#' @return Clean data frame.
#' @export
wash <- function(x) {
    if (!is.data.frame(x)) {
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
    mutate_all(x, funs(replace))
}
