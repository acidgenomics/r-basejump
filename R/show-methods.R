#' Show an object
#' @name show
#' @inherit methods::show
#' @examples
#' data(rse, package = "acidtest")
#' options(acid.test = TRUE)
#'
#' ## EggNOG ====
#' x <- EggNOG()
#' show(x)
#'
#' ## PANTHER ====
#' x <- PANTHER("Homo sapiens", progress = FALSE)
#' show(x)
NULL



## Updated 2019-07-22.
`show,EggNOG` <-  # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            ids = object %>%
                .[["annotations"]] %>%
                .[["eggnogID"]] %>%
                sort(),
            categories = object %>%
                .[["cogFunctionalCategories"]] %>%
                .[["description"]] %>%
                sort()
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
