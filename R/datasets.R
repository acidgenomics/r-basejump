#' Motor Trend Car Road Tests
#'
#' @import datasets
#' @keywords internal
#'
#' @return [data.frame].
#' @export
#'
#' @examples
#' glimpse(mtcars)
datasets::mtcars -> mtcars



#' Starwars Characters
#'
#' @keywords internal
#'
#' @return [tibble].
#' @export
#'
#' @examples
#' glimpse(starwars)
dplyr::starwars -> starwars



#' Miles per Gallon
#'
#' @keywords internal
#'
#' @return [tibble].
#' @export
#'
#' @examples
#' glimpse(mpg)
ggplot2::mpg -> mpg



#' Gene Symbol Synonyms
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @examples
#' names(synonyms)
#' glimpse(synonyms[["homoSapiens"]])
"synonyms"
