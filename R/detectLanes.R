#' Detect Sequencing Lanes
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @param object `character`. Sequencing file or directory paths.
#' @param pattern `string`. Grep pattern. Default pattern checks for `"_LXXX"`.
#'
#' @return `integer`. Lane number.
#'
#' @examples
#' files <- c("sample1_L001.fastq.gz", "sample1_L002.fastq.gz")
#' detectLanes(files)
detectLanes <- function(
    object,
    pattern = lanePattern
) {
    assert_is_character(object)
    assert_is_a_string(lanePattern)
    object <- basename(object)
    if (any(grepl(pattern, object))) {
        as.integer(str_match(object, pattern)[, 2L])
    } else {
        integer()
    }
}
