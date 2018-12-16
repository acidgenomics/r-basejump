#' Get the base name without the file extension
#' @inherit base::basename
#' @export
#' @seealso [basename][base::basename].
#' @examples
#' basenameSansExt("dir/file.txt")
#' basenameSansExt("dir/archive.tar.gz")
basenameSansExt <- function(path) {
    file_path_sans_ext(basename(path), compression = TRUE)
}
