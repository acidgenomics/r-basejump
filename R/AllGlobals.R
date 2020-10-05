#' All global variables
#' @noRd
NULL



globalVariables(".")

## FIXME Use packageName consistently across packages.
.version <- packageVersion(packageName())



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
