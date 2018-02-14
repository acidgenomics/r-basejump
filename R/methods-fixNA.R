#' Fix Character Strings Missing `NA`
#'
#' @rdname fixNA
#' @name fixNA
#' @family Cleanup Utilities
#'
#' @inheritParams general
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' # character
#' fixNA(c(1L, "x", "", "NA", "NULL"))
#'
#' # data.frame
#' data.frame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     stringsAsFactors = FALSE) %>%
#'     fixNA()
#'
#' # tibble
#' tibble(
#'     a = c("foo", ""),
#'     b = c(NA, "bar")) %>%
#'     fixNA()
NULL



# Constructors =================================================================
.fixNA.character <- function(object) {
    assert_is_character(object)
    patterns <- c(
        "^$",
        "^\\s+$",
        "^NA$",
        "^NULL$")
    gsub(paste(patterns, collapse = "|"), NA, object)
}



#' @importFrom dplyr funs mutate_if
.fixNA.tidy <- function(object) {  # nolint
    mutate_if(object, is.character, funs(fixNA))
}



# Methods ======================================================================
#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("ANY"),
    function(object) {
    # Return unmodified by default
    object
})



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("character"),
    .fixNA.character)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("data.frame"),
    .fixNA.tidy)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("DataFrame"),
    function(object) {
        object %>%
            as.data.frame() %>%
            fixNA() %>%
            as("DataFrame")
    })



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("tbl_df"),
    .fixNA.tidy)
