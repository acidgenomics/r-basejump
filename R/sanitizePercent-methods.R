#' @name sanitizePercent
#' @inherit bioverbs::sanitizePercent
#' @inheritParams params
#' @examples
#' object <- c("100%", "10.0%", "1%", "0.1%", "0.01%")
#' class(object)
#' print(object)
#' x <- sanitizePercent(object)
#' class(x)
#' print(x)
NULL



#' @importFrom bioverbs sanitizePercent
#' @aliases NULL
#' @export
bioverbs::sanitizePercent



sanitizePercent.atomic <-  # nolint
    function(object) {
        # Return unmodified.
        object
    }



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("atomic"),
    definition = sanitizePercent.atomic
)



sanitizePercent.character <-  # nolint
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
    signature = signature("character"),
    definition = sanitizePercent.character
)
