#' Package version
#'
#' @note Updated 2020-10-07.
#' @noRd
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
