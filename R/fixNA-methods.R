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
#' df <- data.frame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("c", "d"),
#'     stringsAsFactors = FALSE
#' )
#' fixNA(df)
#'
#' # DataFrame ====
#' DF <- as(df, "DataFrame")
#' fixNA(DF)
NULL



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("ANY"),
    function(object) {
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
        if (has_rownames(object)) {
            rownames <- rownames(object)
        } else {
            rownames <- NULL
        }
        object <- mutate_if(object, is.character, funs(fixNA))
        rownames(object) <- rownames
        object
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
                fixNA(col)
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
