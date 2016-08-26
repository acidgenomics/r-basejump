renderall <- function(...) {
  sapply(
    list.files(pattern = "*.Rmd", full.names = TRUE, recursive = TRUE),
    function(x) {
      rmarkdown::render(x, output_format = "all", ...)
    }
  )
}
