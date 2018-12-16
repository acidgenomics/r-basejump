#' Manipulate file paths
#' @inherit base::basename
#' @export
#' @seealso `basename`.
#' @examples
#' basenameSansExt("dir/file.txt")
#' basenameSansExt("dir/archive.tar.gz")
basenameSansExt <- function(path) {
    file_path_sans_ext(basename(path), compression = TRUE)
}
