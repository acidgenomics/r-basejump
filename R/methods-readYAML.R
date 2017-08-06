#' Read YAML File
#'
#' @rdname readYAML
#' @name readYAML
#'
#' @param object YAML file path.
#'
#' @return [list].
NULL



# Methods ====
#' @rdname readYAML
#' @export
setMethod("readYAML", "character", function(object) {
    if (file.exists(object)) {
        message(paste("Reading", basename(object)))
        yaml.load_file(object)
    } else {
        NULL
    }
})
