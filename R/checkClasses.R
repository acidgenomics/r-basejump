#' Check Classes
#'
#' @inheritParams general
#' @param expected `list`. Named list of expected classes per slot.
#' @param subset `boolean`. Only check a subset of slots in the object.
#' @export
#'
#' @examples
#' checkClasses(
#'     object = metadata(rse_small),
#'     expected = list(
#'         version = c("package_version", "numeric_version"),
#'         date = "Date",
#'         interestingGroups = "character"
#'     )
#' )
checkClasses <- function(
    object,
    expected,
    subset = FALSE
) {
    assert_is_list(expected)
    assert_has_names(expected)
    assert_is_a_bool(subset)
    if (isTRUE(subset)) {
        assert_is_subset(names(expected), names(object))
    } else {
        assert_are_set_equal(names(expected), names(object))
    }
    valid <- mapply(
        slot = names(expected),
        classes = expected,
        MoreArgs = list(object = object),
        FUN = function(slot, classes, object) {
            intersect <- intersect(
                x = classes,
                y = class(object[[slot]])
            )
            if (!has_length(intersect)) {
                FALSE
            } else {
                TRUE
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    if (!all(valid)) {
        stop(paste(
            "Class checks failed.",
            updateMessage,
            printString(valid),
            sep = "\n"
        ))
    }
    TRUE
}
