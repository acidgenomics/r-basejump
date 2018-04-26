#' Fix Character Strings Missing `NA`
#'
#' @name fixNA
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' # character ====
#' fixNA(c(1L, "x", "", "NA", "NULL"))
#'
#' # data.frame ====
#' x <- data.frame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("a", "b"),
#'     stringsAsFactors = FALSE
#' )
#' fixNA(x)
#'
#' # DataFrame ====
#'
NULL



# Methods ======================================================================
#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("ANY"),
    function(object) {
        # Return unmodified by default
        object
    }
)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("character"),
    function(object) {
        patterns <- c(
            "^$",
            "^\\s+$",
            "^NA$",
            "^NULL$",
            "^none available$"
        )
        gsub(paste(patterns, collapse = "|"), NA, object)
    }
)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("data.frame"),
    function(object) {
        object %>%
            rownames_to_column() %>%
            mutate_if(is.character, funs(fixNA)) %>%
            column_to_rownames()
    }
)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("DataFrame"),
    function(object) {
        rownames <- rownames(object)
        list <- lapply(
            X = object,
            FUN = function(col) {
                if (is.character(col)) {
                    fixNA(col)
                } else {
                    col
                }
            })
        DataFrame(list, row.names = rownames)
    }
)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("tbl_df"),
    function(object) {
        mutate_if(object, is.character, funs(fixNA))
    }
)
