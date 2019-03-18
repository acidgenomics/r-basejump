#' @name sanitizeNA
#' @inherit bioverbs::sanitizeNA
#' @export
#' @inheritParams params
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



#' @importFrom bioverbs sanitizeNA
#' @aliases NULL
#' @export
bioverbs::sanitizeNA



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
                if (is.character(col)) {
                    sanitizeNA(col)
                } else {
                    I(col)
                }
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
