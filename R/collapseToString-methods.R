#' @name collapseToString
#' @inherit bioverbs::collapseToString
#' @note Updated 2019-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param sep `character(1)`.
#'   Separator. Defaults to comma.
#' @param unique `logical(1)`.
#'   Unique values.
#' @param sort `logical(1)`.
#'   Sort values.
#' @param removeNA `logical(1)`.
#'   Remove NA values.
#' @param ... Additional arguments.
#'
#' @seealso [`toString()`][base::toString].
#'
#' @return
#' - `atomic`: `character(1)`.
#' - `dim`: Object of same class, collapsed to a single row.
#'
#' @examples
#' ## character ====
#' groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
#' collapseToString(
#'     groceries,
#'     unique = TRUE,
#'     sort = TRUE,
#'     removeNA = TRUE
#' )
#' collapseToString(
#'     groceries,
#'     unique = FALSE,
#'     sort = FALSE,
#'     removeNA = FALSE
#' )
#'
#' ## numeric ====
#' collapseToString(seq(1:5))
#'
#' ## logical ====
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NaN))
#'
#' ## data.frame ====
#' datasets::iris %>%
#'     head() %>%
#'     collapseToString(sort = TRUE, unique = TRUE) %>%
#'     t()
NULL



#' @rdname collapseToString
#' @name collapseToString
#' @importFrom bioverbs collapseToString
#' @usage collapseToString(object, ...)
#' @export
NULL



## Updated 2019-08-18.
`collapseToString,atomic` <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        assert(
            isAny(object, classes = c("character", "factor", "vector")),
            isString(sep),
            isFlag(unique),
            isFlag(sort)
        )
        ## Early return unmodified if scalar.
        if (isScalar(object)) {
            return(object)
        }
        ## Sort, if desired.
        if (isTRUE(sort)) {
            object <- sort(object, na.last = TRUE)
        }
        ## Remove NA values, if desired.
        if (!all(is.na(object))) {
            if (isTRUE(removeNA)) {
                object <- removeNA(object)
            } else {
                object <- sanitizeNA(object)
            }
        }
        ## Make unique, if desired.
        if (isTRUE(unique)) {
            object <- unique(object)
        }
        ## Return.
        out <- as.character(object)
        out <- paste(out, collapse = sep)
        out
    }



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("atomic"),
    definition = `collapseToString,atomic`
)



## Alternatively, can use `dplyr::summarise_all()` approach.
## Updated 2019-08-10.
`collapseToString,matrix` <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        assert(hasLength(object))
        x <- object
        x <- as.data.frame(x, stringsAsFactors = FALSE)
        list <- lapply(
            X = x,
            FUN = function(x) {
                x <- sanitizeNA(x)
                x <- collapseToString(
                    object = x,
                    sep = sep,
                    sort = sort,
                    removeNA = removeNA,
                    unique = unique
                )
            }
        )
        x <- do.call(what = cbind, args = list)
        x <- as.data.frame(x, stringsAsFactors = FALSE)
        x <- as(object = x, Class = class(object)[[1L]])
        x
    }



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("matrix"),
    definition = `collapseToString,matrix`
)



## Updated 2019-07-22.
`collapseToString,data.frame` <-  # nolint
    `collapseToString,matrix`



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("data.frame"),
    definition = `collapseToString,data.frame`
)



## Updated 2019-08-18.
`collapseToString,DataFrame` <-  # nolint
    `collapseToString,data.frame`



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("DataFrame"),
    definition = `collapseToString,DataFrame`
)
