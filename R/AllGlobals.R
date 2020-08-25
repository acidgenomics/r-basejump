#' All global variables
#' @noRd
NULL



globalVariables(".")

.version <- packageVersion("basejump")



#' basejump test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2020-06-09.
#'
#' @examples
#' basejumpTestsURL
basejumpTestsURL <- paste0(
    "https://tests.acidgenomics.com/basejump/",
    "v", .version$major, ".", .version$minor  # nolint
)
