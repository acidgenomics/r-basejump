#' Sanitize `NA` values
#'
#' Standardize empty strings (`""`), character NAs (`"NA"`), and `NULL` values
#' inside a character vector to `NA_character_`. Other `atomic` data types are
#' returned unmodified.
#'
#' @name sanitizeNA
#' @inheritParams params
#' @export
#'
#' @return Modified object.
#' Sanitized to contain proper `NA` values.
#'
#' @examples
#' ## character ====
#' from <- as.character(c(1L, "x", "", "NA", "NULL"))
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
#'
#' ## DataFrame ====
#' from <- S4Vectors::DataFrame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
NULL



sanitizeNA.atomic <-  # nolint
    function(object) {
        # Return unmodified.
        object
    }


#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("atomic"),
    definition = sanitizeNA.atomic
)



# Note that names will be kept here after the gsub call.
sanitizeNA.character <-  # nolint
    function(object) {
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



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("character"),
    definition = sanitizeNA.character
)



sanitizeNA.factor <-  # nolint
    function(object) {
        x <- sanitizeNA(as.character(object))
        levels(x) <- unique(sanitizeNA(levels(object)))
        names(x) <- names(object)
        x
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("factor"),
    definition = sanitizeNA.factor
)



sanitizeNA.data.frame <-  # nolint
    function(object) {
        if (hasRownames(object)) {
            rownames <- rownames(object)
        } else {
            rownames <- NULL
        }
        object <- mutate_if(object, is.character, funs(sanitizeNA))
        rownames(object) <- rownames
        object
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("data.frame"),
    definition = sanitizeNA.data.frame
)



sanitizeNA.DataFrame <-  # nolint
    function(object) {
        rownames <- rownames(object)
        list <- lapply(
            X = object,
            FUN = function(col) {
                sanitizeNA(col)
            })
        DataFrame(list, row.names = rownames)
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("DataFrame"),
    definition = sanitizeNA.DataFrame
)
