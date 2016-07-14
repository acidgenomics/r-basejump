#' Cruft removal for data frame cells
#'
#' @param df Messy data frame -- leading/trailing spaces, dashes
#'
#' @return A reformatted, clean data frame
#' @export
cleanCells <- function(df) {
  # Set cells with only spaces to NA
  df <- data.frame(apply(df, 2, function(x) gsub("^$|^ $", NA, x)))

  # Fix leading and trailing commas
  df <-
    data.frame(apply(df, 2, function(x)
      gsub("^(,|\\s//)\\s(.*)", "\\2", x, perl = TRUE)))
  df <-
    data.frame(apply(df, 2, function(x)
      gsub("(.*)(,|\\s//)\\s$", "\\1", x, perl = TRUE)))

  #! lapply(df, class)
  #! names(df)

  return(df)
}
