#' Assign data
#'
#' Also automatically saves newly assigned object to `data` directory.
#'
#' @param name Desired variable name.
#' @param object Object.
#' @param dir Output directory.
#' @param compress File compression argument from [save()].
#'
#' @export
assignData <- function(name, object, dir = "data", compress = "xz") {
    envir <- parent.frame()
    assign(name, object, envir = envir)
    save(list = name,
         file = file.path(dir, str_c(name, ".rda")),
         envir = envir,
         compress = compress)
}
