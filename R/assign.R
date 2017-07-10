#' Assign and save data
#'
#' @param name Desired variable name.
#' @param object Object.
#' @param compress File compression argument from [save()].
#'
#' @export
assignData <- function(name, object, compress = "xz") {
    assign(name, object, envir = parent.frame())
    save(list = name,
         file = file.path("data", str_c(name, ".rda")),
         compress = compress)
}
