#' Sanitize Percent
#'
#' Take a `character` vector of percentages (e.g. `"50%"`) and convert it to a
#' `numeric` vector (e.g. `0.5`). This function is designed primarily to
#' sanitize data imported from Microsoft Excel.
#'
#' @name sanitizePercent
#' @export
#'
#' @inheritParams general
#'
#' @return `numeric`, if `character` and all items in the vector end with "%".
#'   Otherwise, returns the original object unmodified.
#'
#' @examples
#' object <- c("100%", "10.0%", "1%", "0.1%", "0.01%")
#' class(object)
#' print(object)
#' x <- sanitizePercent(object)
#' class(x)
#' print(x)
NULL



.sanitizePercent.character <-  # nolint
    function(object) {
        if (all(grepl("%$", object))) {
            as.numeric(sub("%$", "", object)) / 100L
        } else {
            object
        }
    }



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("atomic"),
    definition = function(object) {
        object
    }
)



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("character"),
    definition = .sanitizePercent.character
)
