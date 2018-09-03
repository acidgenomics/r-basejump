#' Sanitize `NA` Values
#'
#' Standardize empty strings (`""`), character NAs (`"NA"`), and `NULL` values
#' inside a character vector to `NA_character_`. Other `atomic` data types are
#' returned unmodified.
#'
#' @name sanitizeNA
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' # character ====
#' sanitizeNA(c(1L, "x", "", "NA", "NULL"))
#'
#' # DataFrame ====
#' object <- DataFrame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' sanitizeNA(object)
NULL



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("ANY"),
    definition = function(object) {
        object
    }
)



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("character"),
    definition = function(object) {
        patterns <- c(
            "^$",
            "^\\s+$",
            "^NA$",
            "^NULL$",
            "^none available$"
        )
        gsub(
            pattern = paste(patterns, collapse = "|"),
            replacement = NA,
            x = object
        )
    }
)



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("data.frame"),
    definition = function(object) {
        if (has_rownames(object)) {
            rownames <- rownames(object)
        } else {
            rownames <- NULL
        }
        object <- mutate_if(object, is.character, funs(sanitizeNA))
        rownames(object) <- rownames
        object
    }
)



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("DataFrame"),
    definition = function(object) {
        rownames <- rownames(object)
        list <- lapply(
            X = object,
            FUN = function(col) {
                sanitizeNA(col)
            })
        DataFrame(list, row.names = rownames)
    }
)



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("tbl_df"),
    definition = function(object) {
        mutate_if(object, is.character, funs(sanitizeNA))
    }
)
