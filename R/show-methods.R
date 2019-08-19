#' Show an object
#'
#' @name show
#' @inherit methods::show
#' @note Updated 2019-08-18.
#'
#' @examples
#' options(acid.test = TRUE)
#'
#' ## EggNOG ====
#' x <- EggNOG()
#' show(x)
#'
#' ## PANTHER ====
#' x <- PANTHER("Homo sapiens")
#' show(x)
NULL



#' @rdname show
#' @name show
#' @importFrom methods show
#' @usage show(object)
#' @export
NULL



## Updated 2019-08-18.
`show,EggNOG` <-  # nolint
    function(object) {
        showHeader(object)
        ids <- sort(object[["annotations"]][["eggnogID"]])
        categories <- sort(object[["cogFunctionalCategories"]][["description"]])
        showSlotInfo(list(
            ids = ids,
            categories = categories
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("EggNOG"),
    definition = `show,EggNOG`
)



## Updated 2019-07-22.
`show,PANTHER` <-  # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            organism = metadata(object)[["organism"]],
            release = metadata(object)[["release"]],
            genes = object[["geneID"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = `show,PANTHER`
)
