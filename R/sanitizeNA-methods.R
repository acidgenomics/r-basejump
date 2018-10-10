#' Sanitize `NA` Values
#'
#' Standardize empty strings (`""`), character NAs (`"NA"`), and `NULL` values
#' inside a character vector to `NA_character_`. Other `atomic` data types are
#' returned unmodified.
#'
#' @name sanitizeNA
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' # character ====
#' from <- as.character(c(1L, "x", "", "NA", "NULL"))
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
#'
#' # DataFrame ====
#' from <- DataFrame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
NULL



# atomic =======================================================================
# Return unmodified for generic atomic vectors.
#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("atomic"),
    definition = function(object) {
        object
    }
)



# character ====================================================================
# Note that names will be kept here after the gsub call.
.sanitizeNA.character <-  # nolint
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
    definition = .sanitizeNA.character
)



# factor =======================================================================
.sanitizeNA.factor <-  # nolint
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
    definition = .sanitizeNA.factor
)



# data.frame ===================================================================
.sanitizeNA.data.frame <-  # nolint
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
    definition = .sanitizeNA.data.frame
)



# DataFrame ====================================================================
.sanitizeNA.DataFrame <-  # nolint
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
    definition = .sanitizeNA.DataFrame
)



# tibble =======================================================================
.sanitizeNA.tbl_df <-  # nolint
    function(object) {
        mutate_if(object, is.character, funs(sanitizeNA))
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("tbl_df"),
    definition = .sanitizeNA.tbl_df
)
