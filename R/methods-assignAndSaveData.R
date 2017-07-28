#' Assign and Save Data
#'
#' @rdname assignAndSaveData
#'
#' @param name Desired variable name.
#'
#' @details
#' - [assignAndSaveData()]: Assigns a new object by name to the
#'   current working environment then saves the newly assigned object, specified
#'   by `dir`.
#'
#' @export
setMethod(
    "assignAndSaveData",
    signature(name = "character",
              object = "ANY"),
    function(name, object, dir = "data", compress = TRUE) {
        envir <- parent.frame()
        assign(name, object, envir = envir)
        save(list = name,
             file = file.path(dir, str_c(name, ".rda")),
             envir = envir,
             compress = compress)
    })
